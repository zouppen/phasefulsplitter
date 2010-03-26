import java.io.*;
import java.sql.*;
import java.util.logging.*;
import java.util.Properties;
import java.util.regex.*;
import java.math.BigInteger;

/**
 * Apache-login parsija
 */
public class DatabaseTool {

    private String dbURLIn;
    private String dbURLOut;
    private Properties config = new Properties();
    private Properties dbConfigIn = new Properties();
    private Properties dbConfigOut = new Properties();

    public static Logger logger = Logger.getLogger("splitter");
    private Connection connIn; // database connection for reading data
    private Connection connOut; // database connection for writing data
    private Phase p;
    private long currentID = -1;
    private static final Pattern portionPattern;

    // MySQL doesn't support omitting count in limit, so we need a huge number.
    private static final BigInteger veryBig = new BigInteger("1000000000000");

    // Prepared for speed and convenience.
    private PreparedStatement inStmt, outStmt, errStmt;

    static {
	// Change this if you want different syntax in command line.
	String portionRegEx = "^([\\d]+)-([\\d]+)$";
	
	portionPattern = Pattern.compile(portionRegEx);
    }

    /**
     * Dynamic thingies to be done once per instance
     */
    public DatabaseTool(Phase p) throws Exception {

	final String configFile = "database.conf";
	final String dbInConfigFile = "database_in.conf";
	final String dbOutConfigFile = "database_out.conf";

	// TODO Fix the whole config file handler

	// Some default values for the db
	this.dbConfigIn.setProperty("allowMultiQueries","true");
	this.dbConfigOut.setProperty("allowMultiQueries","true");

	// Reading config (overriding defaults if needed)
	this.config.load(new InputStreamReader(new FileInputStream(configFile),"UTF-8"));
	this.dbConfigIn.load(new InputStreamReader(new FileInputStream(dbInConfigFile),"UTF-8"));
	this.dbConfigOut.load(new InputStreamReader(new FileInputStream(dbOutConfigFile),"UTF-8"));
	
	this.p = p;

	// This is for handling SIGUSR1 signal which produces a nice
	// progress view for long-lasting runs. This uses Oracle's
	// Java extension which may not be a part of your JRE. If so,
	// just comment out next line to disable that feature.
        ProgressSignalHandler.install(this);

	// Opening the database connection
	this.newConnection();
    }

    /**
     * Sets limits to the query to allow running the processing in a
     * distributed environment.
     *
     * @param portionStr The string given in command line.
     * @param startParam The position of start parameter in SQL query.
     * @param endParam The position of end parameter in SQL query.
     */
    public void setPortion(String portionStr, int skipParam, int countParam)
	throws Exception {
	
	// Parsing the output.
	Matcher m = portionPattern.matcher(portionStr);
	if (!m.matches()) throw new Exception("Syntax error in portion string,"
					      +" must be in form start-end");

	BigInteger start = new BigInteger(m.group(1));
	BigInteger end;
	if ("".equals(m.group(2))) {
	    end = veryBig;
	} else {
	    end = new BigInteger(m.group(2));
	}

	// Numbering starts from 1 so skipping one less.
	BigInteger skip = start.add(BigInteger.valueOf(-1));
	BigInteger count = end.subtract(skip);

	inStmt.setObject(skipParam,start);
	inStmt.setObject(countParam,end);

	this.logger.info("Processing only lines from "+start+" to "+end+","+
			 "skip is "+skip+" and count is "+count+".");
    }

    /**
     * Gives a new connection statement. Establishes new connections to the
     * database and prepares statements et cetera.
     */
    private void newConnection() throws SQLException {

	// Two database connections are needed because writing to a
	// connection is impossible while iterating thru SELECT
	// results. Another option is to read all results to memory but
	// that's virtually impossible with because we have gigabytes of
	// data.
	this.connIn =
	    DriverManager.getConnection(dbConfigIn.getProperty("database_uri"),
					dbConfigIn);
	this.connOut =
	    DriverManager.getConnection(dbConfigOut.getProperty("database_uri"),
					dbConfigOut);

	// Prepare insertion and query of rows

	// This dirty hack for row-to-row fetching is presented in
	// http://forums.mysql.com/read.php?39,152636,153012#msg-153012 .
	// That's why here is Integer.MIN_VALUE and other strange things.
	this.inStmt = connIn.prepareStatement(this.p.inStmt,
					      ResultSet.TYPE_FORWARD_ONLY,
					      ResultSet.CONCUR_READ_ONLY);
	this.inStmt.setFetchSize(Integer.MIN_VALUE);
	
	this.outStmt = connOut.prepareStatement(this.p.outStmt);
	this.errStmt = connOut.prepareStatement(this.p.errStmt);
    }
    
    public void processTable() throws Exception{
	
	ResultSet rows = this.inStmt.executeQuery();
	this.currentID = 0;
	
	while (rows.next()) {
	    this.currentID++;
	    try {
		if (p.process(rows,outStmt))
		    outStmt.executeUpdate();
	    } catch (Exception e) {
		// Something failed with processing.
		// If this fails, let the exception fly out.
		this.logger.warning("Row number: "+this.currentID+" failed.");

		if (p.error(rows,e,errStmt))
		    errStmt.executeUpdate();
	    }
	}

	if (this.p.endStmt != null) {
	    this.logger.info("Finalizing...");
	    this.connOut.prepareStatement(this.p.endStmt).executeUpdate();
	}

	this.logger.info("Done! Processed "+this.currentID+" rows.");
    }

    public void printProgress() {
	if (this.currentID == -1)
	    this.logger.info("Initialization not yet ready.");
	else
	    this.logger.info("Processing row number "+this.currentID);
    }
}

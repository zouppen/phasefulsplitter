import java.io.*;
import java.sql.*;
import java.util.logging.*;
import java.util.Properties;

/**
 * Apache-login parsija
 */
public class DatabaseTool {

    private String db_url;
    private Properties db_config = new Properties();
    private int maxReconnects;
    public static Logger logger = Logger.getLogger("splitter");
    private Connection conn; // database connection
    
    // Prepared for speed and convenience.
    private String inStmtStr, outStmtStr, errStmtStr;
    private PreparedStatement inStmt, outStmt, errStmt;

    /**
     * Dynamic thingies to be done once per instance
     */
    public DatabaseTool(String inputQuery, String outputQuery, String errorOutputQuery) throws Exception {

	final String db_file = "database.conf";

	// Some default values
	this.db_config.setProperty("allowMultiQueries","true");

	// Reading config (overriding defaults if needed)
	this.db_config.load(new InputStreamReader(new FileInputStream(db_file),
						  "UTF-8"));

	// Building URI for database
	this.db_url = "jdbc:mysql://" + db_config.getProperty("hostname") + 
	    "/" + db_config.getProperty("database");

	// We are using database.conf for own configuration, too.
	// TODO Maybe this should be in different Properties to avoid
	// name clash.
	this.maxReconnects =
	    Integer.parseInt(db_config.getProperty("max_reconnects"));
	
	// Setting queries
	this.inStmtStr = inputQuery;
	this.outStmtStr = outputQuery;
	this.errStmtStr = errorOutputQuery;

	// Opening database connection
	this.newConnection();
    }

    /**
     * Gives a new connection statement. Establishes a new connection to the
     * database.
     */
    private void newConnection() throws SQLException {
	this.conn = DriverManager.getConnection(db_url,db_config);

	// Prepare insertion and query of rows
	this.inStmt = conn.prepareStatement(inStmtStr);
	this.outStmt = conn.prepareStatement(outStmtStr);
	this.errStmt = conn.prepareStatement(errStmtStr);
    }

    public void processTable(Phase p) throws Exception{
	
	ResultSet rows = this.inStmt.executeQuery();

	while (rows.next()) {
	    try {
		if (p.process(rows,outStmt))
		    outStmt.executeUpdate();
	    } catch (Exception e) {
		// Something failed with processing.
		// If this fails, let the exception fly out.
		p.error(rows,e,errStmt);
		errStmt.executeUpdate();
	    }
	}
    }
 }

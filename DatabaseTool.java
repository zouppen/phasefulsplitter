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
    private Phase p;
    private int currentID = -1;

    // Prepared for speed and convenience.
    private PreparedStatement inStmt, outStmt, errStmt;

    /**
     * Dynamic thingies to be done once per instance
     */
    public DatabaseTool(Phase p) throws Exception {

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
     * Gives a new connection statement. Establishes a new connection to the
     * database.
     */
    private void newConnection() throws SQLException {
	this.conn = DriverManager.getConnection(db_url,db_config);

	// Prepare insertion and query of rows
	this.inStmt = conn.prepareStatement(this.p.inStmt);
	this.outStmt = conn.prepareStatement(this.p.outStmt);
	this.errStmt = conn.prepareStatement(this.p.errStmt);
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
	    this.conn.prepareStatement(this.p.endStmt).executeUpdate();
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

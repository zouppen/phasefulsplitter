import java.io.*;
import java.util.Scanner;
import java.util.NoSuchElementException;
import java.sql.*;
import java.util.zip.GZIPInputStream;
import java.util.regex.*;
import java.util.Properties;
import java.util.logging.*;

/**
 * Apache-login parsija
 */
public class Splitter {

    private static final String filenameRegex =
	"^.*/(.*)/(.*)\\.\\d{4}-\\d{2}-\\d{2}\\.gz";
    Pattern filenamePattern = Pattern.compile(filenameRegex);
    
    private String db_url;
    private Properties db_config = new Properties();
    private int maxReconnects;
    // database id (because ARCHIVE engine doesn't support auto_increment.)
    private int id;
    private int file_counter = 0;
    private static Logger logger = Logger.getLogger("splitter");
    private Connection conn; // database connection
    private PreparedStatement stmt; // Prepared for speed and convenience.

    /**
     * Dynamic thingies to be done once per instance
     */
    public Splitter() throws Exception {

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
	
	this.id = Integer.parseInt(db_config.getProperty("id","0"));
	this.id++;
	
	// Opening database connection
	this.newConnection();
    }

    /**
     * Gives a new connection statement. Establishes a new connection to the
     * database.
     */
    public void newConnection() throws SQLException {
	this.conn = DriverManager.getConnection(db_url,db_config);

	// Prepare insertion of rows
	this.stmt =
	    conn.prepareStatement("INSERT weblog (id,ip,date,server,service,"+
				  "request,response,bytes,referer,browser) "+
				  "values(?,?,?,?,?,?,?,?,?,?)");
	
    }

    public void process(Scanner fileNameScanner) throws Exception{
	String fileName;

	while (true) {
	    try {
		fileName = fileNameScanner.nextLine();
	    } catch (NoSuchElementException foo) {
		// End of file list
		break;
	    }

	    // Print this as progress indicator
	    logger.info("Processing file: #"+ (++this.file_counter)+":"+
			fileName);

	    Matcher matcher = filenamePattern.matcher(fileName);
	    if (!matcher.matches() || matcher.groupCount() != 2) {
		throw new FileNameException(fileName);
	    }
	    
	    String server = matcher.group(1);
	    String service = matcher.group(2);

	    InputStream in =new GZIPInputStream(new FileInputStream(fileName));
	    Scanner scanner = new Scanner(in, "UTF-8");
	    int linenum = 1;
	    String line = "";
	    
	    try {
		while (true) {
		    line = scanner.nextLine();
		    LogLine entry = new LogLine(server,service,line);

		    stmt.setInt(1,this.id); // because there's no auto_increment
		    entry.putFields(this.stmt);
		    this.stmt.execute();
		    
		    linenum++;
		    this.id++;
		}
	    } catch (NoSuchElementException foo) {
		// Tiedosto kaiketi loppu, kaikki ok.
	    } catch (Exception e) {
		logger.severe("Errorneous line is: "+line);
		throw new SyntaxException(fileName,linenum,e);
	    } finally {
		scanner.close();
	    }
	}
    }
    
    public static void main(String args[]) throws Exception {

	Splitter me = new Splitter();
	Scanner fileNameScanner = new Scanner(System.in, "UTF-8");

	logger.info("Initialization ready, starting to read a file list from "+
		    "stdin.");

	// TODO Open an error log and write there...

	try {
	    me.process(fileNameScanner);
	} catch (FileNameException e) {
	    // TODO
	} catch (SyntaxException e) {
	    // TODO
	}

	// Useful information
	logger.info("Successfully processed given files. Last id was "+me.id);
    }
}

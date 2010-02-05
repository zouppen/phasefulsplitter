import java.io.*;
import java.util.Scanner;
import java.util.NoSuchElementException;
import java.sql.*;
import java.util.zip.GZIPInputStream;
import java.util.regex.*;
import java.util.Properties;

/**
 * Apache-login parsija
 */
public class Splitter {

    private static final String filenameRegex =
	"^.*/(.*)/(.*)\\.\\d{4}-\\d{2}-\\d{2}\\.gz";

    private String db_url;
    private Properties db_config = new Properties();
    private int maxReconnects;

    /**
     * Dynamic thingies to be done once per instance
     */
    public Splitter() throws Exception {
	
	final String db_file = "database.conf";

	// Some default values
	this.db_config.setProperty("allowMultiQueries","true");

	// Reading config (overriding defaults if needed)
	this.db_config.load(new InputStreamReader(new FileInputStream(db_file), "UTF-8"));

	// Building URI for database
	this.db_url = "jdbc:mysql://" + db_config.getProperty("hostname") + 
	    "/" + db_config.getProperty("database");

	// We are using database.conf for own configuration, too.
	// TODO Maybe this should be in different Properties to avoid name clash. 
	this.maxReconnects = Integer.parseInt(db_config.getProperty("max_reconnects"));
	
    }

    /**
     * Gives a new connection statement. Establishes a new connection to the
     * database.
     */
    public Connection newConnection() throws SQLException {
	Connection conn = DriverManager.getConnection(db_url,db_config);
	return conn;
    }
    
    public static void main(String args[]) throws Exception {

	Splitter me = new Splitter();
	Connection conn = me.newConnection();
	Pattern filenamePattern = Pattern.compile(filenameRegex);

	// database id (because ARCHIVE engine doesn't support auto_increment.)
	int id = 1;

	// Prepare insertion of rows
	PreparedStatement stmt =
	    conn.prepareStatement("INSERT weblog (id,ip,date,server,service,"+
				  "request,response,bytes,referer,browser) "+
				  "values(?,?,?,?,?,?,?,?,?,?)");

	for (String filename: args) {
	    System.out.println(filename);

	    Matcher matcher = filenamePattern.matcher(filename);
	    if (!matcher.matches() || matcher.groupCount() != 2) {
		throw new Exception("Filename pattern is not clear. Must be hostname/service.year-month-day.gz");
	    }
	    
	    String server = matcher.group(1);
	    String service = matcher.group(2);

	    InputStream in =new GZIPInputStream(new FileInputStream(filename));
	    Scanner scanner = new Scanner(in, "UTF-8");
	    int linenum = 1;
	    String line = "";

	    // Let's prepare INSERT request
	    
	    try {
		while (true) {
		    line = scanner.nextLine();
		    LogLine entry = new LogLine(server,service,line);

		    stmt.setInt(1,id); // because there's no auto_increment
		    entry.putFields(stmt);
		    stmt.execute();
		    
		    linenum++;
		}
	    } catch (NoSuchElementException foo) {
		// Tiedosto kaiketi loppu, kaikki ok.
	    } catch (Exception e) {
		System.err.println("Error at: "+filename+":"+linenum);
		System.err.println("Content: "+line);
		throw e;
	    } finally {
		scanner.close();
	    }
	}
    }
}

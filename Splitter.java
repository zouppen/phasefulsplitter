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

    //private static final String start = "INSERT weblog (server,service,ip,date,request,response,bytes,referer,browser) values ";
    private static final String start_ip = "INSERT IGNORE ip (ip) values ";
    private static final String start_browser = "INSERT IGNORE browser_raw (browser) values ";

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
	
	// Constructing SQL line for default values
	// int cur_game = Integer.parseInt(db_config.getProperty("game_id"));
	// this.sqlInitQuery = "SET @cur_game = "+cur_game+";";
    }

    /**
     * Gives a new connection statement. Establishes a new connection to the
     * database.
     */
    public Statement newConnection() throws SQLException {
	Connection conn = DriverManager.getConnection(db_url,db_config);
	Statement stmt = conn.createStatement();
	//stmt.execute(sqlInitQuery); // set some defaults
	return stmt;
    }
    
    public static void main(String args[]) throws Exception {

	Splitter me = new Splitter();
	Statement stmt = me.newConnection();
	//SQLBuilder sqlstr = new SQLBuilder(start);
	SQLBuilder sqlstr_browser = new SQLBuilder(start_browser);
	SQLBuilder sqlstr_ip = new SQLBuilder(start_ip);
	Pattern filenamePattern = Pattern.compile(filenameRegex);

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
	    
	    try {
		while (true) {
		    line = scanner.nextLine();
		    LogLine entry = new LogLine(server,service,line);
		    //Appender ap_ip = new LineAppender(entry);
		    Appender ap_ip = new IPAppender(entry);
		    Appender ap_browser = new BrowserAppender(entry);

		    //sqlstr.addElement_browser(entry);
		    sqlstr_ip.addElement(ap_ip);
		    sqlstr_browser.addElement(ap_browser);

		    if ((linenum % 100) == 0) {
			String command = sqlstr_ip.toString();
			if (!"".equals(command)) {
			    stmt.executeUpdate(command);
			}

			command = sqlstr_browser.toString();
			if (!"".equals(command)) {
			    stmt.executeUpdate(command);
			}
			
			//sqlstr.clear();
			sqlstr_ip.clear();
			sqlstr_browser.clear();
		    }
		    
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
		
		// One more time, flush the sql buffer
		String command = sqlstr_ip.toString();
		if (!"".equals(command)) {
		    stmt.executeUpdate(command);
		}
		
		command = sqlstr_browser.toString();
		if (!"".equals(command)) {
		    stmt.executeUpdate(command);
		}
		
		//sqlstr.clear();
		sqlstr_ip.clear();
		sqlstr_browser.clear();
	    }
	}
    }

    private static ResultSet getEmptyResult(Statement stmt)
	throws java.sql.SQLException {
	
	ResultSet koe = stmt.executeQuery("select * from weblog limit 0;");
	koe.moveToInsertRow();
	return koe;
    }
}

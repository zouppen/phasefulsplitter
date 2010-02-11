/**
 * Class for processing phase one, reading log files to the
 * database. For more information about the interface, see its
 * documentation.
 */

import java.io.InputStream;
import java.io.FileInputStream;
import java.util.Scanner;
import java.util.NoSuchElementException;
import java.util.regex.*;
import java.util.zip.GZIPInputStream;
import java.sql.ResultSet;
import java.sql.PreparedStatement;

public class LogLinePhase extends Phase {
    
    private static final String fileNameRE =
	"^.*/(.*)/(.*)\\.\\d{4}-\\d{2}-\\d{2}\\.gz";
    private static final Pattern fileNamePattern = Pattern.compile(fileNameRE);
    private static final int fileNameGroups = 2;

    // Public attributes for getting SQL queries
    public LogLinePhase() {
	inStmt = "SELECT text from file";
	outStmt = "INSERT phase_2_data (server,service,line) VALUES(?,?,?)";
	errStmt = "INSERT phase_1_error (error,file) VALUES (?,?)";
    }

    /**
     * Reads a file list.
     *
     * @param in Input row to process
     * @param out Output row to produce. Please note this may contain
     *            old data. You should reset all fields.
     * @returns True if it has set statement ready for excecution.
     */
    public boolean process(ResultSet in, PreparedStatement out) throws Exception {
	
	String fileName = in.getString(1);
	Matcher matcher = fileNamePattern.matcher(fileName);
	if (!matcher.matches() || matcher.groupCount() != fileNameGroups) {
	    throw new FileNameException(fileName);
	}
	    
	out.setString(1,matcher.group(1)); // server name
	out.setString(2,matcher.group(2)); // service name
	
	InputStream inSt = new GZIPInputStream(new FileInputStream(fileName));
	Scanner scanner = new Scanner(inSt, "UTF-8");
	int linenum = 1;
	String line = "";
	    
	try {
	    while (true) {
		line = scanner.nextLine();
		
		out.setString(3,line);
		out.executeUpdate(); // Push a new line to the database.
		
		linenum++;
	    }
	} catch (NoSuchElementException foo) {
	    // Tiedosto kaiketi loppu, kaikki ok.
    	} finally {
	    scanner.close();
	}
	
	return false; // Statements are already executed.
    }

    /**
     * In case of an error in insertion. 
     *
     * @param in Input row which contains the row that was tried to process.
     * @param out Error row to produce. Please note that this may
     *        contain old data. You should reset all fields.
     * @returns Always true because it has a new row ready.
     */
    public boolean error(ResultSet in, Exception e, PreparedStatement err) throws Exception {
	if (e instanceof FileNameException) {
	    // This error can be post-processed
	    err.setString(1,e.getMessage());
	    err.setString(2,in.getString(1));
	    return true;
	}

	// Otherwise it dies.
	throw new Exception("Fatal error when reading a file",e);
    }

    public static void main(String[] args) throws Exception {

	DatabaseTool tool = new DatabaseTool(new LogLinePhase());
	tool.processTable();
    }
}

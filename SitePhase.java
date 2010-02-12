/**
 * Class for processing phase one, reading log files names and
 * extracting site information from file names. For more information
 * about the interface, see its documentation.
 */

import java.util.regex.*;
import java.sql.ResultSet;
import java.sql.PreparedStatement;

public class SitePhase extends Phase {
    
    private static final String fileNameRE =
	"^.*/(.*)/(.*)\\.\\d{4}-\\d{2}-\\d{2}\\.gz";
    private static final Pattern fileNamePattern = Pattern.compile(fileNameRE);
    private static final int fileNameGroups = 2;

    // Public attributes for getting SQL queries
    public SitePhase() {
	inStmt = "SELECT file from phase_1_data";
	outStmt = "INSERT phase_2_data (server,service,file) VALUES(?,?,?)";
	errStmt = "INSERT DELAYED phase_1_error (error,file) VALUES (?,?)";
	endStmt = "INSERT site (server,service) "+
	    "SELECT DISTINCT (server,service) from phase_2_data";
    }

    /**
     * Reads a file list.
     *
     * @param in Input row to process
     * @param out Output row to produce. Please note this may contain
     *            old data. You should reset all fields.
     * @returns True if it has set statement ready for execution.
     */
    public boolean process(ResultSet in, PreparedStatement out) throws Exception {
	
	String fileName = in.getString(1);
	Matcher matcher = fileNamePattern.matcher(fileName);
	if (!matcher.matches() || matcher.groupCount() != fileNameGroups) {
	    throw new FileNameException(fileName);
	}
	    
	out.setString(1,matcher.group(1)); // server name
	out.setString(2,matcher.group(2)); // service name
	out.setString(3,fileName); // file name

	return true; // New statement ready.
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

	DatabaseTool tool = new DatabaseTool(new SitePhase());
	tool.processTable();
    }
}

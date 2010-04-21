/**
 * Class for processing phase three, parsing log file lines in the
 * database. For more information about the interface, see its
 * documentation.
 */

import java.sql.ResultSet;
import java.sql.PreparedStatement;

public class ParsingPhase extends Phase {
    
    // Public attributes for getting SQL queries
    public ParsingPhase() {
	inStmt = "SELECT site_id,line from phase_3_data LIMIT ?,?";
	outStmt = "INSERT DELAYED phase_4_data (site_id,ip,date,"+
	                 "request,response,bytes,referer,browser) "+
	                 "values(?,?,?,?,?,?,?,?)";
	errStmt = "INSERT DELAYED phase_3_error (error,site_id,line) "+
	                 "VALUES (?,?,?)";
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
	
	String line = in.getString(2);
       	LogLine parsed = new LogLine(line);
	
	out.setInt(1,in.getInt(1)); // site_id
	parsed.putFields(out); // set other fields
	
	return true;
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

	err.setString(1,e.getMessage());
	err.setInt(2,in.getInt(1));
	err.setString(3,in.getString(2));

	return true;
    }

    public static void main(String[] args) throws Exception {

	DatabaseTool tool = new DatabaseTool(new ParsingPhase());

	if (args.length == 1) {
	    tool.setPortion(args[0],1,2);
	} else if (args.length > 1) {
	    throw new Exception("Too many parameters.");
	}
	
	tool.processTable();
    }
}

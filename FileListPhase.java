/**
 * Class for processing phase zero, reading the file list. For more
 * information about the interface, see its documentation.
 */

import java.io.FileInputStream;
import java.util.Scanner;
import java.util.NoSuchElementException;
import java.sql.ResultSet;
import java.sql.PreparedStatement;

public class FileListPhase extends Phase {
    
    // Public attributes for getting SQL queries
    private String inStmt =
	"\"SELECT \"log_access.txt\"";
    private String outStmt =
	"INSERT phase_1_data (file) VALUES(?)";
    private String errStmt =
	"INSERT phase_0_error ()";

    /**
     * Reads a file list.
     *
     * @param in Input row to process
     * @param out Output row to produce. Please note this may contain
     *            old data. You should reset all fields.
     * @returns True if it has set statement ready for excecution.
     */
    public boolean process(ResultSet in, PreparedStatement out) throws Exception {
	String listFileName = in.getString(1);
	Scanner fileListScanner =
	    new Scanner(new FileInputStream(listFileName), "UTF-8");
	
	try {
	    while (true) {
		String filename = fileListScanner.nextLine();
		out.setString(1,filename);
		out.executeQuery();
	    }
	} catch (NoSuchElementException foo) {
	    // End of file list reached, everything is OK.
	}   

	return false; // Statements are already executed.
    }

    /**
     * In case of an error in insertion. 
     *
     * @param in Input row which contains the row that was tried to process.
     * @param out Error row to produce. Please note that this may
     *        contain old data. You should reset all fields.
     */
    /*public void error(ResultSet in, Exception e, PreparedStatement err) throws Exception {
	// No need to cope with errors, aborting
	throw new Exception("Error adding line to the database",e);
	}*/

    public static void main(String[] args) throws Exception {

	DatabaseTool tool = new DatabaseTool(new FileListPhase());
	tool.processTable();
    }
}

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

    String inFile;
    
    // Public attributes for getting SQL queries
    public FileListPhase(String inFile) {
	inStmt = "SELECT 'nothing'";
	outStmt = "INSERT DELAYED phase_1_data (file) VALUES(?)";
	errStmt = "INSERT phase_0_error ()";
	this.inFile = inFile;
    }

    /**
     * Reads a file list.
     *
     * @param in Input row to process
     * @param out Output row to produce. Please note this may contain
     *            old data. You may want to reset all fields.
     * @returns True if it has set statement ready for excecution.
     */
    public boolean process(ResultSet in, PreparedStatement out) throws Exception {
	Scanner fileListScanner =
	    new Scanner(new FileInputStream(this.inFile), "UTF-8");
	
	try {
	    while (true) {
		String filename = fileListScanner.nextLine();
		out.setString(1,filename);
		out.executeUpdate();
	    }
	} catch (NoSuchElementException foo) {
	    // End of file list reached, everything is OK.
	}   

	return false; // Statements are already executed.
    }

    public static void main(String[] args) throws Exception {

	DatabaseTool tool = new DatabaseTool(new FileListPhase(args[1]));
	tool.processTable();
    }
}

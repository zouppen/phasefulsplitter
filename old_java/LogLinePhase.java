/**
 * Class for processing phase two, reading log files to the
 * database. For more information about the interface, see its
 * documentation.
 */

import java.io.InputStream;
import java.io.FileInputStream;
import java.util.Scanner;
import java.util.NoSuchElementException;
import java.util.zip.GZIPInputStream;
import java.sql.ResultSet;
import java.sql.PreparedStatement;

public class LogLinePhase extends Phase {
    
    // Public attributes for getting SQL queries
    public LogLinePhase() {
	inStmt =
	    "SELECT service.id as service_id, server.id as server_id, file "+
	    "FROM phase_2_data as phase, service, server "+
	    "WHERE phase.server=server.name and phase.service=service.name";
	outStmt = "INSERT INTO phase_3_data (service,server,line) VALUES(?,?,?)";
	errStmt = "INSERT INTO phase_2_error (error,service,server,file) VALUES (?,?,?,?)";

	//when reprocessing in case of an error, uncomment and fix these these (FIXME)
	//inStmt = "SELECT site_id as id,file from phase_2_error";
	//errStmt = "INSERT DELAYED phase_2_error_error (error,site_id,file) VALUES (?,?,?)";
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
	
	String fileName = in.getString(3);
	    
	InputStream inSt = new GZIPInputStream(new FileInputStream(fileName));
	Scanner scanner = new Scanner(inSt, "UTF-8");
	String line = "";
	
	// Server and service ID's stay the same.
	out.setInt(1,in.getInt(1));
	out.setInt(2,in.getInt(2));

	try {
	    while (true) {
		line = scanner.nextLine();
		
		out.setString(3,line);
		out.executeUpdate(); // Push a new line to the database.
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
	if (e instanceof java.io.FileNotFoundException) {
	    // This error can be post-processed
	    err.setString(1,e.getMessage());
	    err.setInt(2,in.getInt(1));
	    err.setInt(3,in.getInt(2));
	    err.setString(4,in.getString(3));
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

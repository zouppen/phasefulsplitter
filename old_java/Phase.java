/**
 * Interface for processing one phase. Does something interesting with
 * SQL row and produces a new row. If something has failed,
 * DatabaseTool will call "error" method.
 */

import java.sql.ResultSet;
import java.sql.PreparedStatement;

public abstract class Phase {

    // Java doesn't support final in abstract classes. These should be final.
    public String inStmt; // Query which return all relevant rows and columns
    public String outStmt; // Update or insert statement used in process()
    public String errStmt; // Same as with above but with error()
    public String endStmt; // Query to execute at the end of the updates
                           // (optional).

    /**
     * This is called every time a new line has been read. Takes a row
     * in and sets out values as it finds it best.
     *
     * @param in Input row to process
     * @param out Output row to produce. Please note this may contain
     *            old data. You should reset all fields.
     * @returns True if it has set statement ready for execution.
     */
    abstract boolean process(ResultSet in, PreparedStatement out) throws Exception;

    /**
     * This is called when processing or the insert query after that
     * has failed and the new result is not inserted to the new
     * database. This should copy all necessary information to "err"
     * to allow reprocessing of the failed lines. If this is not
     * implemented, processing terminates.
     *
     * @param in Input row which contains the row that was tried to process.
     * @param out Error row to produce. Please note that this may
     *        contain old data. You should reset all fields.
     * @returns True if it has set error statement ready for execution.
     */
    public boolean error(ResultSet in, Exception e, PreparedStatement err) throws Exception {
	// No need to cope with errors, aborting
	throw new Exception("Error processing a row",e);
    }
}

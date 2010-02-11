/**
 * Interface for processing one phase. Does something interesting with
 * SQL row and produces a new row. If something has failed,
 * DatabaseTool will call "error" method.
 */

import java.sql.ResultSet;
import java.sql.PreparedStatement;

public abstract class Phase {

    private String inStmt;
    private String outStmt;
    private String errStmt;

    // Some SQL query strings
    public String getInStmt() {return inStmt;}
    public String getOutStmt() {return outStmt;}
    public String getErrStmt() {return errStmt;}


    /**
     * This is called every time a new line has been read. Takes a row
     * in and sets out values as it finds it best.
     *
     * @param in Input row to process
     * @param out Output row to produce. Please note this may contain
     *            old data. You should reset all fields.
     * @returns True if it has set statement ready for excecution.
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
     */
    public void error(ResultSet in, Exception e, PreparedStatement err) throws Exception {
	// No need to cope with errors, aborting
	throw new Exception("Error processing a row",e);
    }
}

/**
 * Interface for processing one phase. Does something interesting with
 * SQL row and produces a new row. If something has failed,
 * DatabaseTool will call "error" method.
 */

import java.sql.ResultSet;
import java.sql.PreparedStatement;

public interface Phase {

    /**
     * This is called every time a new line has been read. Takes a row
     * in and sets out values as it finds it best.
     *
     * @param in Input row to process
     * @param out Output row to produce. Please note this may contain
     *            old data. You should reset all fields.
     * @returns True if it has set statement ready for excecution.
     */
    public boolean process(ResultSet in, PreparedStatement out);

    /**
     * This is called when processing or the insert query after that
     * has failed and the new result is not inserted to the new
     * database. This should copy all necessary information to "err"
     * to allow reprocessing of the failed lines.
     *
     * @param in Input row which contains the row that was tried to process.
     * @param out Error row to produce. Please note that this may
     *        contain old data. You should reset all fields.
     */
    public void error(ResultSet in, Exception e, PreparedStatement err);
}

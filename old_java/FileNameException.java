import java.lang.Exception;

public class FileNameException extends Exception {

    FileNameException(String file) {
	this(file,null);
    }

    FileNameException(String file, Throwable cause) {
	super("Error in file name pattern. Should be in form of 'hostname/service.year-month-day.gz' but is: "+file,cause);
    }
}

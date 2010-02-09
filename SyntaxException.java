import java.lang.Exception;

public class SyntaxException extends Exception {

    public String file;
    public int line;
    
    SyntaxException(String file, int line) {
	this(file,line,null);
	this.line = line;
    }

    SyntaxException(String file, int line, Throwable cause) {
	super("Syntax error in "+file+":"+line,cause);
	this.line = line;
    }

}

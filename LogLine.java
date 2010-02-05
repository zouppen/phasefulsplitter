import java.util.Locale;
import java.util.regex.*;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.text.ParsePosition;
import java.lang.StringBuilder;
import java.sql.PreparedStatement;

public class LogLine {

    private static final String logEntryRegEx = "^([\\d\\.]+) (\\S+) (\\S+) \\[([\\w:/]+\\s[+\\-]\\d{4})\\] \"(.+?)\" (\\d{3}) (\\d+|-) \"(.*)\" \"(.*)\"$";
    private static final int logEntryGroups = 9;
    private static final Pattern logEntryPattern;

    private static final DateFormat apacheFormat;
    private static final DateFormat outputFormat;
    
    // Keep in same order as in Splitter for clarity
    // (id),ip,date,server,service,request,response,bytes,referer,browser
    public String ip;
    public Date date;
    public String server;
    public String service;
    public String request;
    public Integer response;
    public Integer bytes;
    public String referer;
    public String browser;

    static {
	logEntryPattern = Pattern.compile(logEntryRegEx);
	apacheFormat = new SimpleDateFormat("dd/MMM/yyy:HH:mm:ss Z",
					    Locale.ENGLISH);
	outputFormat = DateFormat.getInstance();
    }

    public LogLine(String server, String service,String line)
	throws Exception {
	
	this.server = server;
	this.service = service;

	ParsePosition position = new ParsePosition(0);

	Matcher matcher = logEntryPattern.matcher(line);
	if (!matcher.matches() || logEntryGroups != matcher.groupCount()) {
	    throw new Exception("syntax error.");
	}

	this.ip = matcher.group(1);
	this.date = apacheFormat.parse(matcher.group(4),position);
	if (position.getErrorIndex() != -1)
	    throw new Exception("syntax error in date format.");
	this.request = matcher.group(5);
	this.response = new Integer(matcher.group(6));
	if (!"-".equals(matcher.group(7))) // else null
	    this.bytes = new Integer(matcher.group(7));
	if (!(matcher.group(8).equals("-") ||
	      matcher.group(8).equals("")))
	    this.referer = matcher.group(8);
	if (!(matcher.group(9).equals("-") ||
	      matcher.group(9).equals("")))
	    this.browser = matcher.group(9);
    }

    public void putFields(PreparedStatement stmt) throws Exception {
	// (id),ip,date,server,service,request,response,bytes,referer,browser
	// 1st: id
	stmt.setObject(2,ip);
	stmt.setObject(3,date);
	stmt.setObject(4,server);
	stmt.setObject(5,service);
	stmt.setObject(6,request);
	stmt.setObject(7,response);
	stmt.setObject(8,bytes);
	stmt.setObject(9,referer);
	stmt.setObject(10,browser);
    }

    public String engineerDebug() {
	return "IP: "+ this.ip +
	    "\nDate: " + outputFormat.format(this.date) +
	    "\nBrowser: " + this.browser;
    }    
}

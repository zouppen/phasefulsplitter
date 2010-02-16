import java.util.Locale;
import java.util.regex.*;
import java.util.Date;
import java.util.TimeZone;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.sql.PreparedStatement;

public class LogLine {

    private static final String logEntryRegEx = "^([\\d\\.]+) (\\S+) +(\\S+) +\\[([\\w:/]+\\s[+\\-]\\d{4})\\] \"(.+?)\" (\\d{3}) (\\d+|-) \"(.*)\" \"(.*)\"$";
    private static final int logEntryGroups = 9;
    private static final Pattern logEntryPattern;

    private static final DateFormat apacheFormat;
    private static final DateFormat sqlFormat;
    
    // Keep in same order as in database for clarity
    // (site),ip,date,request,response,bytes,referer,browser
    public String ip;
    public Date date;
    public String request;
    public Integer response;
    public Integer bytes;
    public String referer;
    public String browser;

    static {
	logEntryPattern = Pattern.compile(logEntryRegEx);
	apacheFormat = new SimpleDateFormat("dd/MMM/yyy:HH:mm:ss Z",
					    Locale.ENGLISH);
	// Fail if date is not correct.
	apacheFormat.setLenient(false);
	
	// In addition to format change, also convert time zone to UTC.
	sqlFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
					 Locale.ENGLISH);
	sqlFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    public LogLine(String line)
	throws Exception {
	
	Matcher matcher = logEntryPattern.matcher(line);
	if (!matcher.matches() || logEntryGroups != matcher.groupCount()) {
	    throw new Exception("Syntax error.");
	}

	this.ip = matcher.group(1);
	this.date = apacheFormat.parse(matcher.group(4));
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
	// (site),ip,date,request,response,bytes,referer,browser
	// 1st: site
	stmt.setObject(2,ip);
	stmt.setString(3,sqlFormat.format(date)); // Mind the time zone.
	stmt.setObject(4,request);
	stmt.setObject(5,response);
	stmt.setObject(6,bytes);
	stmt.setObject(7,referer);
	stmt.setObject(8,browser);
    }
}

import sun.misc.Signal;
import sun.misc.SignalHandler;

public class ProgressSignalHandler implements SignalHandler {

    private SignalHandler oldHandler;
    private static Signal sigUSR = new Signal("USR2");
    private DatabaseTool callback;
    
    /**
     * Installs signal handler for USR1 signal. Callback function
     * inside DatabaseTool is called when SIGUSR1 is trapped.
     *
     * @param callback An object to call when SIGUSR1 is trapped.
     * @returns A new SignalHandler. 
     */
    public static SignalHandler install(DatabaseTool callback) {
        ProgressSignalHandler instance = new ProgressSignalHandler();
        instance.oldHandler = Signal.handle(sigUSR, instance);
	instance.callback = callback;
        return instance;
    }
    
    /**
     * Called automatically by JRE.
     */
    public void handle(Signal signal) {
	
        try {
	    if (sigUSR.equals(signal))
		callback.printProgress();

            // Chain back to previous handler, if one exists
            if (oldHandler != SIG_DFL && oldHandler != SIG_IGN) {
                oldHandler.handle(signal);
            }
        } catch (Exception e) {
            System.err.println("Signal handler failed, reason " + e.getMessage());
            e.printStackTrace();
        }
    }
}

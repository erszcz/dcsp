package pl.edu.agh.dcsp;

public class LogMessage {
	
	public enum Type{
		IS_OK, AGENT_VIEW, INCONSISTENT, ADJUSTED, FIND_NOGOOD, SEND_NOGOOD, DONE, RECEIVED;
	}

	Type type;
	int sender;
	int receiver;
	String content;
	
	int oldPos;
	int newPos;
	
}

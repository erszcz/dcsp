package pl.edu.agh.dcsp;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LogParser {
	
	List<String> log;
	private int N;
	int at=-1;
	
	void readLogFile(){
		try {
			at=-1;
			log = Files.readAllLines(Paths.get("dcsp.log"), Charset.defaultCharset());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	
	void setProblemSize(int n){
		this.N=n;
	}

	int[] getInitialPositions(){
		
		Pattern ptrn = Pattern.compile("\\{\\d+,\\{(.*)\\}\\}");
		
		int[] ret = new int[N];
		for(int i =0; i< N; i++){
			String s = log.get(i);
			Matcher matcher = ptrn.matcher(s);
			matcher.find();
			String found = matcher.group(1);
			
			String[] coords = found.split(",");
			int row = Integer.parseInt(coords[0])-1;
			int col = Integer.parseInt(coords[1])-1;
			//System.out.println(N+" "+row+","+col);
			ret[row]=col;
		}
		
		return ret;
	}
	
	String getInitialPositionsAsString(){
		
		StringBuilder sb = new StringBuilder();
		
		for(int i =0; i< N; i++){
			String s = log.get(i);
			sb.append(s);
			sb.append("\n");
		}
		return sb.toString();
	}

	
	public void removeUnused() {
		
		for(int i = 0; i<N; i++){
			// remove every log about initial positions
			// for easier traversal later on
			log.remove(0);
		}
		
		/**
		Pattern receivePtrn = Pattern.compile("<<");
		Iterator<String> it = log.iterator();
		
		while(it.hasNext()){
			String s = it.next();
			// ignore lines containing "<<"
			// they just say that a previously sent message arrived
			Matcher matcher = receivePtrn.matcher(s);
			if(matcher.find()){
				it.remove();
			}
		}
		*/
	}
	
	
	
	public LogMessage parseNext(){
	
		at++;
		LogMessage ret = parseAt();
		return ret;
	}


	public LogMessage parsePrev() {
		at--;
		LogMessage ret=parseAt();
		return ret;
	}
	
	
	private LogMessage parseAt(){
		String s = log.get(at);	
		
	//	System.out.println(s);
		
		if(s.contains("! {is_ok")){	
			//System.out.println(" sent is_ok");
			String[] splt = s.split(" ");
			int from = Integer.parseInt(splt[0].replace("[", "").replace("]", ""))-1;
			int to = Integer.parseInt(splt[1])-1;
			
			LogMessage lm = new LogMessage();
			lm.type=LogMessage.Type.IS_OK;
			lm.sender=from;
			lm.receiver=to;
			lm.content = s;
			
			// this is generally done for the visualiser
			
			return lm;
		}
			

		if(s.contains("inconsistent")){
			///System.out.println("inconsistent");
			String[] splt = s.split(" ");
			int from = Integer.parseInt(splt[0].replace("[", "").replace("]", ""))-1;
			
			LogMessage lm = new LogMessage();
			lm.type=LogMessage.Type.INCONSISTENT;
			lm.sender = from;
			
			// this is also done
			
			lm.content = s;
			
			return lm;
		}
		
		if(s.contains("adjusted")){
			//System.out.println("adjusted");
			String[] splt = s.split(" ");
			String fromStr = splt[0].replace("[", "").replace("]", "");
			int from = Integer.parseInt(fromStr)-1;
			
			
			StringBuffer sb = new StringBuffer(s);

			int newPosIndex = sb.lastIndexOf("New");
			int it = newPosIndex;
			int found =0;
			int index0=-1;
			int index1=-1;
			
			while(found<2){
				char curr = sb.charAt(it);
				if(found==0){
					if(curr==','){
						found=1;
						index0=it+1;
					} else {
						it--;
						continue;					
					}
				}
				
				if(curr=='}'){
					found=2;
					index1=it;

				} else {
					it++;
				}
			}
			
			int oldIndex = Integer.parseInt(sb.substring(index0,index1));
			//System.out.println(sb.substring(index0-2, index1));
			
			
			// do the same thing for the new index index			
			it = sb.length()-1;
			found =0;
			index0=-1;
			index1=-1;
			
			while(found<2){
				char curr = sb.charAt(it);
				if(found==0){
					if(curr==','){
						found=1;
						index0=it+1;

					} else {
						it--;
						continue;					
					}
				}				
				if(curr=='}'){
					found=2;
					index1=it;
				} else {
					it++;
				}
			}
			
			int newIndex = Integer.parseInt(sb.substring(index0,index1));
			
			
			LogMessage lm = new LogMessage();
			lm.type=LogMessage.Type.ADJUSTED;
			lm.sender = from;
			lm.oldPos=oldIndex-1;
			lm.newPos=newIndex-1;
			
			lm.content = s;
			
			return lm;
		}
		
		
		if(s.contains("agent view")){
			
		//	System.out.println("agent view");
			String[] splt = s.split(" ");
			int from = Integer.parseInt(splt[0].replace("[", "").replace("]", ""))-1;
			
			LogMessage lm = new LogMessage();
			lm.type=LogMessage.Type.AGENT_VIEW;
			lm.sender = from;
			
			// yeah, this too
			
			lm.content = s;
			
			return lm;
		}
		
			
		if(s.contains("nogood:")){
			//System.out.println("nogood:");
			String[] splt = s.split(" ");
			String fromStr = splt[0].replace("[", "").replace("]", "");
			int from = Integer.parseInt(fromStr)-1;
			
			LogMessage lm = new LogMessage();
			lm.type=LogMessage.Type.FIND_NOGOOD;
			lm.sender = from;			
			lm.content = s;
			
			// TODO find_nogood: --> does it need more?
			
			return lm;
		}
				
		if(s.contains("! {nogood")){
			///System.out.println(" sent nogood");
			String[] splt = s.split(" ");
			int from = Integer.parseInt(splt[0].replace("[", "").replace("]", ""))-1;
			int to = Integer.parseInt(splt[1])-1;
			
			LogMessage lm = new LogMessage();
			lm.type=LogMessage.Type.SEND_NOGOOD;
			lm.sender = from;
			lm.receiver = to;
			lm.content = s;
			
			return lm;
		}
		
		if(s.contains("done")){
			///System.out.println("{nogood");
			String[] splt = s.split(" ");
			int from = Integer.parseInt(splt[0].replace("[", "").replace("]", ""))-1;
			int to = Integer.parseInt(splt[1])-1;
			
			LogMessage lm = new LogMessage();
			lm.type=LogMessage.Type.DONE;
			lm.sender = from;
			lm.receiver = to;
			lm.content = s;
			
			return lm;
		}
		
		
		if(s.contains("<<")){
			
			String[] splt = s.split(" ");
			int from = Integer.parseInt(splt[0].replace("[", "").replace("]", ""))-1;
			LogMessage lm = new LogMessage();
			lm.type=LogMessage.Type.RECEIVED;
			lm.sender = from;
			
			// USING newPos to remember receiving line id
			// and oldPos to remember sending line id 
			// associated with the sent message
			lm.newPos = at;
			lm.oldPos = at-1;
			
			StringBuffer sb = new StringBuffer(s);
			int refBeginIndex = sb.lastIndexOf("#");
			
			int refEndIndex = sb.lastIndexOf(">")+1;
			String refStr = s.substring(refBeginIndex, refEndIndex);
			//System.out.println(refStr);
			
			while(!log.get(lm.oldPos).contains(refStr)){
				lm.oldPos--;
			}
			
			lm.content = s;
			
			return lm;
		}
		
		
		return null;
	}
}

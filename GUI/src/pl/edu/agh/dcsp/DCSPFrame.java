package pl.edu.agh.dcsp;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.ScrollPane;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;

public class DCSPFrame extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public DCSPFrame(String string) {
		super(string);
	}

	private JPanel leftSide;
	private JScrollPane checkerboardScrollPane;
	private BoardPanel boardPanel;
	private JButton newSimButton;
	private JButton runButton;
	private JPanel buttonContainerL;

	private JPanel rightSide;
	private DCSPLogTextArea textlog;
	private JScrollPane textareaScrollPane;
	private JButton prevButton;
	private JButton nextButton;
	private JButton pauseButton;
	private JButton playButton;
	private JPanel buttonContainerR;

	private int width = 1024;
	private int height = 768;
	
	private LogParser logParser = new LogParser();
	
	
	private boolean first = false;

	public void init() {

		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// setSize(width, height);
		setPreferredSize(new Dimension(width, height));
		getContentPane().setLayout(new GridLayout(1, 2));

		// Build the left side of the window
		leftSide = new JPanel();
		leftSide.setLayout(new GridBagLayout());
		GridBagConstraints c1 = new GridBagConstraints();

		boardPanel = new BoardPanel();
		boardPanel.setWindowWidth(width);
		// boardPanel.setProblemSize(8);

		checkerboardScrollPane = new JScrollPane(boardPanel);
		c1.fill = GridBagConstraints.BOTH;
		c1.insets = new Insets(10, 10, 10, 10);
		c1.weightx = 1.0;
		c1.weighty = 0.9;
		c1.gridwidth = 3;
		c1.anchor = GridBagConstraints.PAGE_START;
		leftSide.add(checkerboardScrollPane, c1);
		checkerboardScrollPane.revalidate();

		buttonContainerL = new JPanel();
		newSimButton = new JButton("New Simulation");
		buttonContainerL.add(newSimButton);
		runButton = new JButton("Run");
		runButton.setEnabled(false);
		buttonContainerL.add(runButton);
		c1.fill = GridBagConstraints.NONE;
		c1.weighty = 0.1;
		c1.anchor = GridBagConstraints.SOUTHWEST;
		c1.insets = new Insets(10, 10, 10, 10);
		c1.gridwidth = 1;
		c1.gridy = 2;

		leftSide.add(buttonContainerL, c1);

		getContentPane().add(leftSide);

		// Build the right side of the window
		rightSide = new JPanel();
		rightSide.setLayout(new GridBagLayout());
		GridBagConstraints c2 = new GridBagConstraints();
		textlog = new DCSPLogTextArea();
		textlog.setText("Click New Simulation to start...\n");
		textlog.setEnabled(false);
		textlog.setEditable(false);
		DefaultCaret caret = (DefaultCaret)textlog.getCaret();
		caret.setUpdatePolicy(DefaultCaret.NEVER_UPDATE); // suppresses autoscroll on setText
		textareaScrollPane = new JScrollPane(textlog);
		c2.fill = GridBagConstraints.BOTH;
		c2.insets = new Insets(10, 10, 10, 10);
		c2.weightx = 1.0;
		c2.weighty = 0.9;
		c2.gridx = 0;
		c2.gridwidth = 3;
		c2.anchor = GridBagConstraints.PAGE_START;
		rightSide.add(textareaScrollPane, c2);

		buttonContainerR = new JPanel();
		
		pauseButton = new JButton("Pause");
		pauseButton.setEnabled(false);
		playButton = new JButton("Play");
		playButton.setEnabled(false);
		JSeparator sep = new JSeparator(JSeparator.VERTICAL);
		buttonContainerR.add(pauseButton);
		buttonContainerR.add(playButton);
		buttonContainerR.add(sep);
		
		prevButton = new JButton("Prev");
		prevButton.setEnabled(false);
		buttonContainerR.add(prevButton);
		nextButton = new JButton("Next");
		nextButton.setEnabled(false);
		buttonContainerR.add(nextButton);
		
		

		c2.fill = GridBagConstraints.NONE;
		c2.weighty = 0.1;
		c2.anchor = GridBagConstraints.SOUTHEAST;
		c2.insets = new Insets(10, 10, 10, 10);
		c2.gridwidth = 1;
		c2.gridy = 2;
		c2.gridx = 2;

		rightSide.add(buttonContainerR, c2);
		getContentPane().add(rightSide);

		newSimButton.addActionListener(new NewSimActionListener());
		runButton.addActionListener(new RunActionListener());
		nextButton.addActionListener(new NextActionListener());
		prevButton.addActionListener(new PrevActionListener());
		playButton.addActionListener(new PlayActionListener());
		pauseButton.addActionListener(new PauseActionListener());
		
		pack();
		setVisible(true);
	}

	private class NewSimActionListener implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent e) {

			int result = -1;
			try {
				//TODO NewSimActionListener back to production mode
				/*
				result = Integer.parseInt(JOptionPane.showInputDialog(
						DCSPFrame.this,
						"Enter new simulation size (a positive integer):"));
				*/
				
				//temporarily
				result=4;

				if (result > 0) {

					textlog.setText("Click the checkerboard to set up the initial conditions, then press run...");
					boardPanel.setProblemSize(result);
					logParser.setProblemSize(result);
					boardPanel.setInteractiveMode(true);
					
					DCSPFrame.this.pack();
					boardPanel.revalidate();
					checkerboardScrollPane.revalidate();
					
					playButton.setEnabled(false);
					pauseButton.setEnabled(false);
					runButton.setEnabled(true);
					prevButton.setEnabled(false);
					nextButton.setEnabled(false);
					

				} else {

					JOptionPane.showMessageDialog(DCSPFrame.this,
							"Invalid input");
				}

			} catch (NumberFormatException nfe) {
				JOptionPane.showMessageDialog(DCSPFrame.this, "Invalid input");
			}

		}
	}

	private class RunActionListener implements ActionListener {

		//TODO RunActionListener production mode
		@Override
		public void actionPerformed(ActionEvent e) {
			//System.out.println(boardPanel.getCommandString());
/*
 * 			THIS FRAGMENT WORKS, IT'LL BE UNCOMMENTED LATER
 * 			I just want an unchanging log to be testing on
 * 
			try {
				Process erl = Runtime.getRuntime().exec("cmd /C escript dcsp.escript nqueens1.problem");

				BufferedReader stdInput = new BufferedReader(
						new InputStreamReader(erl.getInputStream()));
				BufferedReader stdError = new BufferedReader(
						new InputStreamReader(erl.getErrorStream()));

				erl.waitFor();

				String results = "";
				String s;

				while ((s = stdInput.readLine()) != null) {
					//System.out.println(s);
					results = results + s;
				}
				// read any errors from the attempted command
				// System.out.println("Here is the standard error of the command (if any):\n");
				while ((s = stdError.readLine()) != null) {
					//System.out.println(s);
					results = results + s;
				}
				
				textlog.setText(results);

			} catch (IOException | InterruptedException e1) {
				e1.printStackTrace();
			}
*/
			
			// read the log and initiate the "film"
			logParser.readLogFile();
			textlog.setEnabled(true);
			textlog.setText("Click Next and Prev to browse the solving process...\n\n");
			textlog.append("The complete initial conditions are:\n");
			textlog.append(logParser.getInitialPositionsAsString());

			boardPanel.storeMessage(null);
			boardPanel.setPositions(logParser.getInitialPositions());
			logParser.removeUnused();
			//prevButton.setEnabled(true);
			
			playButton.setEnabled(true);
			nextButton.setEnabled(true);
			first = true;
			pauseButton.setEnabled(false);
			runButton.setEnabled(false);
		}
	}
	
	
	private void handleLogMsg(LogMessage m){
		
		boardPanel.storeMessage(m);
		
		//textlog.setText(m.content);
		textlog.removeHighlights();
		//System.out.println(logParser.at+"/"+logParser.log.size());
		
		if(m.type==LogMessage.Type.RECEIVED){
			textlog.highlightLine(m.oldPos, Color.CYAN);
		}
		textlog.highlightLine(logParser.at, Color.LIGHT_GRAY);
		try {
			textlog.setCaretPosition(textlog.getLineStartOffset(logParser.at));
		} catch (BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	private void displaySolutionLog(){
		
		textlog.setText(logParser.log.get(0));			
		for(int i =1; i< logParser.log.size(); i++){
			
			textlog.append("\n");
			textlog.append(logParser.log.get(i));
		}
		textlog.append("\n");
		first = false;
	}		
	
	private class NextActionListener implements ActionListener{		

		@Override
		public void actionPerformed(ActionEvent arg0) {
			
			if(first){				
				displaySolutionLog();
			}
			
			boardPanel.advance();
			LogMessage m = logParser.parseNext();
		
			handleLogMsg(m);
			
			// the last line of the log is the DONE log
			playButton.setEnabled(logParser.at!=logParser.log.size()-1);
			nextButton.setEnabled(logParser.at!=logParser.log.size()-1);
			prevButton.setEnabled(logParser.at>0);
		}

	}
	
	
	private class PrevActionListener implements ActionListener{

		@Override
		public void actionPerformed(ActionEvent e) {
		
			LogMessage m = logParser.parsePrev();
			handleLogMsg(m);
			
			// the last line of the log is the DONE log
			playButton.setEnabled(logParser.at!=logParser.log.size()-1);
			nextButton.setEnabled(logParser.at!=logParser.log.size()-1);
			prevButton.setEnabled(logParser.at>0);		
		}
	}
	
	Timer timer;
	
	//TODO implement play and pause operations on timers
	private class PlayActionListener implements ActionListener{

		@Override
		public void actionPerformed(ActionEvent arg0) {
			nextButton.setEnabled(false);
			prevButton.setEnabled(false);
			pauseButton.setEnabled(true);
			playButton.setEnabled(false);
			timer = new Timer();
			timer.schedule(new TimerTask(){
				@Override
				public void run() {
					if(first){				
						displaySolutionLog();
					}				
					boardPanel.advance();
					LogMessage m = logParser.parseNext();
					handleLogMsg(m);
					if(logParser.at==logParser.log.size()-1){
						timer.cancel();
						prevButton.setEnabled(true);
						nextButton.setEnabled(false);
						pauseButton.setEnabled(false);
					}
				}
			}, 0,1*100); // 0 delay, 1 time per 0.1s rate
			
			
		}
	}
	
	private class PauseActionListener implements ActionListener{

		@Override
		public void actionPerformed(ActionEvent e) {
						
			timer.cancel();
			pauseButton.setEnabled(false);
			playButton.setEnabled(logParser.at!=logParser.log.size()-1);
			nextButton.setEnabled(logParser.at!=logParser.log.size()-1);
			prevButton.setEnabled(logParser.at>0);
		}
	}
}

package pl.edu.agh.dcsp;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.ScrollPane;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;

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
	private JPanel buttonContainerR;

	private int width = 1024;
	private int height = 768;

	public void init() {

		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		//setSize(width, height);
		setPreferredSize(new Dimension(width,height));
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
		textlog.setText("Set up a simulation to fill this box...");
		textlog.setEnabled(false);
		textlog.setEditable(false);
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

		pack();
		setVisible(true);
	}

	private class NewSimActionListener implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent e) {

			int result = -1;
			try {
				result = Integer.parseInt(JOptionPane.showInputDialog(
						DCSPFrame.this,
						"Enter new simulation size (a positive integer):"));
				
				if(result>0){
					
					boardPanel.setProblemSize(result);
					boardPanel.setInteractiveMode(true);
					runButton.setEnabled(true);
					DCSPFrame.this.pack();
					boardPanel.revalidate();
					checkerboardScrollPane.revalidate();
					
					
				} else {
					
					JOptionPane.showMessageDialog(DCSPFrame.this, "Invalid input");
				}

			} catch (NumberFormatException nfe) {
				JOptionPane.showMessageDialog(DCSPFrame.this, "Invalid input");
			}

		}

	}
	
	private class RunActionListener implements ActionListener{

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println(boardPanel.getCommandString());
			
			// TODO orchestrate cooperation with Erlang agent module
		}
	}

}

package pl.edu.agh.dcsp;

import java.awt.BorderLayout;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingConstants;

public class DCSPMain {
	
	
	
	public static void main(String args[]){
		
		//java tutorials say this is for thread safety
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
        		DCSPFrame frame = new DCSPFrame("N-Queens problem");
        		frame.init();
            }
        });
		

	}

}

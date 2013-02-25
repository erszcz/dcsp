package pl.edu.agh.dcsp;

import java.awt.Color;
import java.util.List;

import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter;

public class DCSPLogTextArea extends JTextArea {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	List<String> log;

	public DCSPLogTextArea() {
		super();
	}


	
	public void highlightLine(int line){
		
		DefaultHighlighter h = (DefaultHighlighter) this.getHighlighter();
		try {
			h.removeAllHighlights();
			DefaultHighlightPainter redHighlight = new DefaultHighlighter.DefaultHighlightPainter(
					Color.LIGHT_GRAY);
			
			int start = this.getLineStartOffset(line);
            int end = this.getLineEndOffset(line);
			
			h.addHighlight(start, end, redHighlight);
		} catch (BadLocationException ex) {
			System.out.println("CAN'T HIGHLIGHT");
		}
	}

}

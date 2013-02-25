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


	
	public void highlightLine(int line, Color col){
		
		DefaultHighlighter h = (DefaultHighlighter) this.getHighlighter();
		try {
			
			DefaultHighlightPainter hp = new DefaultHighlighter.DefaultHighlightPainter(
					col);
			
			int start = this.getLineStartOffset(line);
            int end = this.getLineEndOffset(line);
			
			h.addHighlight(start, end, hp);
		} catch (BadLocationException ex) {
			System.out.println("CAN'T HIGHLIGHT");
		}
	}
	
	public void removeHighlights(){
		DefaultHighlighter h = (DefaultHighlighter) this.getHighlighter();
		h.removeAllHighlights();
	}

}

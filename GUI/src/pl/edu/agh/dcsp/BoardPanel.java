package pl.edu.agh.dcsp;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JPanel;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;

public class BoardPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	public BoardPanel() {
		super();
		addMouseListener(new BoardPanelMouseListener());
	}

	private int N = 0;

	private int MIN_CELL = 24;
	private int componentSize;
	private int cellSize;
	private boolean interactive = false;

	private LogMessage storedMessage=null;
	//Set<Point> queens = new HashSet<Point>();
	
	int[] queens;

	private void drawQueens(Graphics g) {
		int r = cellSize / 2;

		for (int i=0; i<N; i++) {
			g.setColor(Color.RED);
			g.fillOval(r / 2 + queens[i] * cellSize, r / 2 + i * cellSize, r, r);
			g.setColor(Color.BLACK);
			g.drawOval(r / 2 + queens[i] * cellSize, r / 2 + i * cellSize, r, r);
		}
	}

	private void drawBoard(Graphics graphics) {
		for (int row = 0; row < N; row++) {
			for (int column = 0; column < N; column++) {
				graphics.setColor((row + column) % 2 == 0 ? Color.GRAY
						: Color.WHITE);
				graphics.fillRect(column * cellSize, row * cellSize, cellSize,
						cellSize);
			}
		}
	}

	public Dimension getPreferredScrollableViewportSize() {
		return getPreferredSize();
	}

	public void paintComponent(Graphics g) {
		g.setColor(Color.LIGHT_GRAY);
		g.fillRect(0, 0, getWidth(), getHeight());
		if (N != 0) {
			drawBoard(g);
			drawQueens(g);
			if(storedMessage!=null){
				visualizeMessage(g);
			}
		}

	}
	
	void drawArrow(Graphics g1, int x1, int y1, int x2, int y2) {
        Graphics2D g = (Graphics2D) g1.create();
        g.setStroke(new BasicStroke(3));
        double dx = x2 - x1, dy = y2 - y1;
        double angle = Math.atan2(dy, dx);
        int len = (int) Math.sqrt(dx*dx + dy*dy);
        AffineTransform at = AffineTransform.getTranslateInstance(x1, y1);
        at.concatenate(AffineTransform.getRotateInstance(angle));
        g.transform(at);

        // Draw horizontal arrow starting in (0, 0)
        len = len-cellSize/4;
        g.drawLine(cellSize/4, 0, len-5, 0);
        g.fillPolygon(new int[] {len, len-10, len-10, len},
                      new int[] {0, -6, 5, 0}, 4);
    }

	private void visualizeMessage(Graphics g) {
		
		int r = cellSize /2;
		int sen = storedMessage.sender;
		int rec = storedMessage.receiver;
		
		int senX = r + queens[sen] * cellSize;
		int senY = r + sen * cellSize;
		
		int recX = r + queens[rec] * cellSize;
		int recY = r + rec * cellSize;
		
		//r / 2 + queens[i] * cellSize, r / 2 + i * cellSize, r, r
		int newPosX = r/2+storedMessage.newPos*cellSize;
		int newPosY = r/2+sen*cellSize;
		
		Font font = new Font("Arial", Font.BOLD, 16);
		g.setFont(font);
		
		switch(storedMessage.type){
		case IS_OK:
			g.setColor(Color.BLUE);
			drawArrow(g, senX, senY, recX, recY);
			g.drawChars("is_ok?".toCharArray(), 0, 6, senX-r/3, senY+r/8);
			break;
		case AGENT_VIEW:
			g.setColor(Color.GREEN);
			g.drawChars("???".toCharArray(), 0, 3, senX-r/4, senY+r/8);
			break;
		case INCONSISTENT:
			g.setColor(Color.YELLOW);
			g.drawChars("!!!".toCharArray(), 0, 3, senX-r/8, senY+r/8);
			break;
		case ADJUSTED:
			g.setColor(Color.PINK);
			g.fillOval(newPosX, newPosY, r,r);
			g.setColor(Color.ORANGE);
			drawArrow(g, senX,senY,newPosX+r/2,newPosY+r/2);
			break;
		case FIND_NOGOOD:
			g.setColor(Color.BLACK);
			g.drawChars("X".toCharArray(), 0, 1, senX, senY+r/8);
			break;
		case SEND_NOGOOD:
			g.setColor(Color.MAGENTA);
			drawArrow(g, senX, senY, recX, recY);
			g.drawChars("nogood".toCharArray(), 0, 6, senX-r/3, senY+r/8);
			break;
		case DONE:
			for (int i=0; i<N; i++) {
				g.setColor(Color.GREEN);
				g.fillOval(r / 2 + queens[i] * cellSize, r / 2 + i * cellSize, r, r);
				g.setColor(Color.BLACK);
				g.drawOval(r / 2 + queens[i] * cellSize, r / 2 + i * cellSize, r, r);
			}
			break;
		}
	}

	public void setWindowWidth(int width) {

		// the 31 was established based solely on
		// my computer; it might not work as I intended it to work
		// on a different look&feel
		// (this'll result in nothing more but unnecessary scrollbars
		// at window initialization and resizing)
		componentSize = width / 2 - 31;

		// repainting before the window is created
		// has no actual effect on anything
		repaint();
	}

	public void setProblemSize(int n) {

		this.N = n;

		cellSize = componentSize / n;
		if (cellSize < MIN_CELL) {
			cellSize = MIN_CELL;
		}
		queens = new int[N];
		for(int i =0; i<N; i++){
			queens[i]=-1;
		}
		setPreferredSize(new Dimension(N * cellSize, N * cellSize));

		repaint();
	}

	public void setInteractiveMode(boolean b) {
		interactive = b;
	}

	private class BoardPanelMouseListener implements MouseListener {

		@Override
		public void mouseClicked(MouseEvent e) {

		}

		@Override
		public void mouseEntered(MouseEvent arg0) {

		}

		@Override
		public void mouseExited(MouseEvent arg0) {

		}

		@Override
		public void mousePressed(MouseEvent arg0) {
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			Point p = e.getPoint();			
			if (interactive && p.x < N * cellSize && p.y < N * cellSize) {
				int cellMiddleX = p.y / cellSize;
				int cellMiddleY = p.x / cellSize;
				//Point candidate = new Point(cellMiddleX, cellMiddleY);
				if (queens[cellMiddleX]==cellMiddleY) {
					queens[cellMiddleX]=-1;
				} else {
					queens[cellMiddleX] = cellMiddleY;
				}
			}
			repaint();
		}
	}

	public String getCommandString(){
	
		StringBuilder sb = new StringBuilder();
		
		sb.append("N= "+N+"\n");
		
		int toRandom=0;
		for(int i =0; i<N; i++){
			if(queens[i]!=-1){
				sb.append("["+i+","+queens[i]+"]\n");
			} else {
				toRandom++;
			}
		}
		
		if(toRandom!=0){
			sb.append("wylosuj sobie jeszcze "+(toRandom));
		}
		
		return sb.toString();
	}
	
	
	public void setPositions(int[] positions){
		
		queens=positions;
		repaint();
	}

	public void advance() {
		
		if(storedMessage!=null && storedMessage.type==LogMessage.Type.ADJUSTED){
			queens[storedMessage.sender]=storedMessage.newPos;
		}
		//repaint();
		
	}

	public void storeMessage(LogMessage m) {
		storedMessage=m;
		if(storedMessage.type==LogMessage.Type.ADJUSTED){
			queens[storedMessage.sender]=storedMessage.oldPos;
		}
		repaint();
	}
}
package pl.edu.agh.dcsp;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
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

	Set<Point> queens = new HashSet<Point>();

	private void drawQueens(Graphics g) {
		int r = cellSize / 2;

		for (Point q : queens) {
			g.setColor(Color.red);
			g.fillOval(r / 2 + q.x * cellSize, r / 2 + q.y * cellSize, r, r);
			g.setColor(Color.black);
			g.drawOval(r / 2 + q.x * cellSize, r / 2 + q.y * cellSize, r, r);
		}
	}

	private void drawBoard(Graphics graphics) {
		for (int row = 0; row < N; row++) {
			for (int column = 0; column < N; column++) {
				graphics.setColor((row + column) % 2 == 0 ? Color.white
						: Color.black);
				graphics.fillRect(column * cellSize, row * cellSize, cellSize,
						cellSize);
			}
		}
	}

	public Dimension getPreferredScrollableViewportSize() {
		return getPreferredSize();
	}

	public void paintComponent(Graphics g) {
		g.setColor(Color.gray);
		g.fillRect(0, 0, getWidth(), getHeight());
		if (N != 0) {
			drawBoard(g);
			drawQueens(g);
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
		queens.clear();
		setPreferredSize(new Dimension(N * cellSize, N * cellSize));

		repaint();
	}

	public void setInteractiveMode(boolean b) {
		interactive = b;
	}

	private class BoardPanelMouseListener implements MouseListener {

		@Override
		public void mouseClicked(MouseEvent e) {

			Point p = e.getPoint();			
			if (interactive && p.x < N * cellSize && p.y < N * cellSize) {
				int cellMiddleX = p.x / cellSize;
				int cellMiddleY = p.y / cellSize;
				Point candidate = new Point(cellMiddleX, cellMiddleY);
				if (queens.contains(candidate)) {
					queens.remove(candidate);
				} else if (queens.size() < N) {
					queens.add(candidate);
				}
			}
			repaint();
		}

		@Override
		public void mouseEntered(MouseEvent arg0) {
			// TODO Auto-generated method stub

		}

		@Override
		public void mouseExited(MouseEvent arg0) {
			// TODO Auto-generated method stub

		}

		@Override
		public void mousePressed(MouseEvent arg0) {
			// TODO Auto-generated method stub

		}

		@Override
		public void mouseReleased(MouseEvent arg0) {
			// TODO Auto-generated method stub

		}
	}

	public String getCommandString(){
	
		StringBuilder sb = new StringBuilder();
		
		sb.append("mam "+queens.size()+"\n");
		
		for(Point q : queens){
			sb.append("["+q.x+","+q.y+"]\n");
		}
		
		if(queens.size()<N){
			sb.append("wylosuj sobie jeszcze "+(N-queens.size()));
		}
		
		return sb.toString();
	}
}
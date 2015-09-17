package kawa;

import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.mapping.*;
import gnu.expr.Language;
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

/** A Frame containing a Kawa read-eval-print loop.
  * @author Albert Ting <alt@artisan.com> (original base)
  * @author Per Bothner (extensive changes).
  */

public class GuiConsole extends JFrame
  implements ActionListener, ReplDocument.DocumentCloseListener {
  private static String CLOSE = "Close";
  private static String EXIT = "Exit";
  private static String NEW = "New";
  private static String NEW_SHARED = "New (Shared)";
  private static String PURGE_MESSAGE = "Purge Buffer";

  static int window_number = 0;

  ReplPane pane;
  ReplDocument document;

  public static void main(String[] args) {
    InPort.noConsole = false;
    int iArg = repl.processArgs(args, 0, args.length);
    repl.getLanguage();
    repl.setArgs(args, iArg);
    repl.checkInitFile();
    new GuiConsole();
  }

  public GuiConsole()
  {
    this(Language.getDefaultLanguage(), Environment.getCurrent(), false);
  }

  public GuiConsole (ReplDocument doc)
  {
    super("Kawa");
    init(doc);
  }

  void init(ReplDocument doc)
  {
    document = doc;
    document.addDocumentCloseListener(this);
    pane = new ReplPane(document);
    window_number++;
    this.setLayout(new BorderLayout(0,0));
    this.add("Center", new JScrollPane(pane));
    setupMenus();
    //pack();
    setLocation(100 * window_number, 50 * window_number);
    setSize(700,500);
    setVisible(true);
  }

  public GuiConsole(Language language, Environment penvironment, boolean shared)
  {
    super("Kawa");
    repl.getLanguage(); // In case a new GuiConsole is created from Java.
    init(new ReplDocument(language, penvironment, shared));

    // Uncomment to test same ReplDocument in two JFrames.
    // new GuiConsole(document);
  }

  public void closed (ReplDocument doc)
  {
    close();
  }

  void close () {
    document.removeDocumentCloseListener(this);
    dispose();
  }

  private void setupMenus() {
    MenuBar menubar;
    Menu fileMenu;
    Menu utilitiesMenu;

    MenuItem menuItem;

    WindowListener windowExitCmd = new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	close();
      }
    };

    // Create the menubar
    menubar = new MenuBar();
    fileMenu = new Menu("File");
    utilitiesMenu = new Menu("Utilities");
   
    menubar.add(fileMenu);
    menubar.add(utilitiesMenu);
    
    menuItem = new MenuItem(NEW);
    menuItem.addActionListener(this);
    fileMenu.add(menuItem);

    menuItem = new MenuItem(NEW_SHARED);
    menuItem.addActionListener(this);
    fileMenu.add(menuItem);

    menuItem = new MenuItem(CLOSE);
    menuItem.addActionListener(this);
    fileMenu.add(menuItem);

    menuItem = new MenuItem(EXIT);
    menuItem.addActionListener(this);
    this.addWindowListener(windowExitCmd);	
    fileMenu.add(menuItem);

    menuItem = new MenuItem(PURGE_MESSAGE);
    menuItem.addActionListener(this);	
    utilitiesMenu.add(menuItem);

    this.setMenuBar(menubar);
  }

  public void actionPerformed(ActionEvent e)
  {
    String cmd = e.getActionCommand();

    if (cmd.equals(NEW))
      new GuiConsole(document.language, Environment.getGlobal(), false);
    else if (cmd.equals(NEW_SHARED))
      new GuiConsole(document.language, document.environment, true);
    else if (cmd.equals(EXIT))
      System.exit(0);
    else if (cmd.equals(CLOSE))
      close();
    else if (cmd.equals(PURGE_MESSAGE)) {
      pane.document.deleteOldText();
    }
    else
      OutPort.outDefault().println("Unknown menu action: "+cmd);
  }
}

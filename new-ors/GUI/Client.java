/*
 * Created on 9 June 2012 by Ruixiao Yu
 *
 * This is part of ORS.  Any subsequent modification
 * of the file should retain this disclaimer.
 */

import java.awt.*;
import java.awt.event.*;
import java.net.*;
import se.sics.prologbeans.*;

import javax.swing.*;

/**
 * Client: the Java GUI of ORS
 *
 * @author Ruixiao Yu
 * 
 */
public class Client implements ActionListener {

    private JTextArea text = new JTextArea(20, 40);
    private JTextField input = new JTextField(36);
    private JButton evaluate = new JButton("Enter");
    private PrologSession session = new PrologSession();
    Thread thread;

    private class SessionThread extends Thread {

        public void run() {

            boolean connection = false;
            while (!connection) {
                boolean exception = false;
                try {
                    session.connect();
                } catch (Exception e) {
                    exception = true;
                }
                if (!exception) {
                    connection = true;
                }
            }

            getQuery();
        }
    }

    public Client() throws java.io.IOException {
        text.setLineWrap(true);
        text.setWrapStyleWord(true);
        if ((Integer.getInteger("se.sics.prologbeans.debug", 0)).intValue() != 0) {
            session.setTimeout(0);
        }
        JFrame frame = new JFrame("ORS User Client");
        Container panel = frame.getContentPane();
        panel.add(new JScrollPane(text), BorderLayout.CENTER);
        JPanel inputPanel = new JPanel(new BorderLayout());
        inputPanel.add(input, BorderLayout.CENTER);
        inputPanel.add(evaluate, BorderLayout.EAST);
        panel.add(inputPanel, BorderLayout.SOUTH);
        text.setEditable(false);
        evaluate.addActionListener(this);
        input.addActionListener(this);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();

        frame.setVisible(true);

        thread = new SessionThread();
        thread.start();
    }

    private void getQuery() {
        String message;
        try {
            QueryAnswer answer =
                    session.executeQuery("query(R,S)");
            PBTerm result1 = answer.getValue("R");
            PBTerm result2 = answer.getValue("S");
            if (result1 != null) {
                message = "Please enter the value for: " + result1.getString() + '\n';
                message += result1.getString() + " is an argument of " + result2.getString() + '\n';
                text.append(message);
                input.setText("");
            } else {
                message = "Error: " + answer.getError() + '\n';
                text.append(message);
            }
        } catch (Exception e) {
            message = "Error when querying Prolog Server: "
                    + e.getMessage() + '\n';
            text.append(message);
            e.printStackTrace();
        }
    }

    public void actionPerformed(ActionEvent event) {
        String message;
        String inputText = input.getText().trim();

        if (inputText.equals("")) {
            return;
        }

        try {
            if (!inputText.startsWith("'")) {
                inputText = "'" + inputText;
            }
            if (inputText.endsWith(".")) {
                inputText = inputText.substring(0, inputText.length() - 2);
            }
            if (!inputText.endsWith("'")) {
                inputText = inputText + "'";
            }
            Bindings bindings = new Bindings().bind("I",
                    inputText + '.');
            QueryAnswer answer =
                    session.executeQuery("evaluate(I)", bindings);
            if (answer.getError() == null) {
                message = inputText + '\n';
                text.append(message);
                input.setText("");

            } else {
                message = "Error: " + answer.getError() + '\n';
                text.append(message);
            }
        } catch (Exception e) {
            message = "Error when evaluation Prolog Server: "
                    + e.getMessage() + '\n';
            text.append(message);
            e.printStackTrace();
        }

        try {
            QueryAnswer answer =
                    session.executeQuery("shutdown");


        } catch (Exception e) {
            message = "Error when shutting down Prolog Server: "
                    + e.getMessage() + '\n';
            text.append(message);
            e.printStackTrace();
        }

        session.disconnect();
        thread = null;

        thread = new SessionThread();
        thread.start();
    }

    public static void main(String[] args) throws java.io.IOException {
        new Client();
    }
}
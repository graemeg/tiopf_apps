
                    tiSQLEditor
                    ===========

tiSQLEditor is a multi-SQL tool for use with various relational database
management systems (SQL database servers). This version of tiSQLEditor
is designed to work with the fpGUI Toolkit. It also has some additional
features that the original tiSQLEditor doesn't - for example the "Run Script
Sequence" feature that is useful for running version based scripts to
upgrade a database. Handy in deployed environments.


Setup
-----
Edit the connections.ini file to define one or more database connections.
The format is:

    <connection name>=<server name>:<database name or alias>

See the included connections.ini file for examples. The connections.ini file
must be in the same directory as the sqleditor executable.


Usage
-----
When running the app, you will be prompted with a database login. Select the
database connection and fill in the user/password details.

Once connected, you are in the main SQL editor window. Write you SQL statement
and press F8 to execute it. If you have multiple SQL statements in the editor,
then select the statement you want to execute, then press F8 - only the selected
SQL statement will be executed.

The results will appear in a separate window.


Tips
----
 - Right click in the editor window and you will see various copy and paste
   options. eg: You can copy a SQL statement as a query string, ready to
   paste into your sourc code. The reverse is also possible.
 - Ctrl+Space will show you a "code completion" window.
   Inside this window, Ctrl+Enter will insert currently selected table name.
   Alt+Enter will insert the type sql statement defined in the combobox.
 - Running update scripts: The main window has a option under "Query >
   Run a script sequence". This is handy if you need to run a set up update
   scripts stored in numerical directories. See the 'updates' directory for
   the directory layout, and suggestioned script file names. The "Check" button
   will list what scripts will be run and in the order that they will be run -
   but not actually execute them.
 - The sql results window has many extra functions for use with tiOPF, where
   it can generate partial tiOPF code based on the result set. This is from
   class definitions to hard-coded visitors.


                           ---------==========----------

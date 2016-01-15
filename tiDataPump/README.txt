tiDataPump
==========

This is a basic application that allows you to copy data from one
persistence layer to another. You specify the list of tables to
be copied, supply the log-in details and click the "Copy Data"
button.

This unitily was originally writte by Peter Hinrichsen for tiOPF1,
but in 2014 I converted it to tiOPF2, and ported it to the cross-platform
fpGUI Toolkit framework.


Requirements
------------

 * tiOPF2
 * The Free Pascal Compiler v2.6.4 or later
 * fpGUI Toolkit (though a console version will be created soon)


Setup
-----

Simply configure the tiOPF to include the persistence layers you are
interested in. This is done via the LINK_xxx compiler defines found in
the tiOPF.lpk package under "Options -> Other". Then simply compile the
project as usual.


Usage
-----

 * Run the tiDataPump application.
 * Specify the Source and Target persistence layers. eg: Firebird -> XMLLight
 * Specify the login criteria if needed.
 * Specify a list of tables to be copied. IMPORTANT: One table name per line.
 * Click the "Copy Data"


                    --------------- oOo ----------------


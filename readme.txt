
-----------------------
tiOPFMapper
-----------------------

DESCRIPTION

tiOPF is a wonderful framework, but the tasks of writing all of the boiler plate
code can be a lot of work.  So I wrote the tiMapper utility in the spirit of
some existing PHP frameworks that use YML or XML to describle a project's classes
and other types which are then transformed into the base class files with all of
hard coded boiler plate code already written.

The tiMapper utility uses one or more xml documents to describe a "schema".  The schema
describes any TtiObject based classes and enumerations as well as mappings that use
the tiAutoMap registration mechanism to store meta data about the types described
in the schema.  There is one main "schema" file which can have INCLUDES pointing to
other xml files which describe yet additional classes, enums and mappings if necessary.

Eventually, I'll write a GUI utility front end for it, but for now I am content writing
out the xml.  A typical 200 line xml schema will produce about 2K lines or more of pascal
code for defining the classes, their visitors, registering visitors, mappings, etc and
gluing everything up so that you're bascially able to just start writing code.

The utility uses the schema to create unit (.pas) files and within the unit files, each
classe, enumeration mappings, etc described in the schema.  Take the following class
definition.

<class
    base-class="TPerson"
    base-class-parent="TtiObject"
    auto-map="true"
    auto-create-list="true">
    <!-- Class properties -->
    <class-props>
        <prop name="FirstName" type="string"/>
        <prop name="LastName" type="string"/>
        <prop name="Age" type="Integer"/>
        <prop name="Gender" type="TGenderType"/>
        <prop name="IsActive" type="Boolean"/>
        <prop name="ActiveDate" type="TDateTime"/>
        <prop name="Email"/>
    </class-props>
    <!-- Mapping into the tiOPF framework -->
    <mapping table="person" pk="OID" pk-field="OID" oid-type="string">
        <prop-map prop="FirstName" field="first_name" type="string"/>
        <prop-map prop="LastName" field="last_name" type="string"/>
        <prop-map prop="Age" field="age"  type="integer"/>
        <prop-map prop="Gender" field="gender" type="enum"/>
    </mapping>
</class>

{ Generated Class: TPerson}
  TPerson = class(TtiObject)
  protected
    // getters, setters, private prop vars
  public
    procedure   Read; override;
    procedure   Save; override;
  published
    property    FirstName: string read FFirstName write SetFirstName;
    property    LastName: string read FLastName write SetLastName;
    property    Age: Integer read FAge write SetAge;
    property    Gender: TGenderType read FGender write SetGender;
    property    IsActive: Boolean read FIsActive write SetIsActive;
    property    ActiveDate: TDateTime read FActiveDate write SetActiveDate;
    property    Email: string read FEmail write SetEmail;
  end;

Based on this description, the tiMapper utility will create a class called TPerson
with all of the appropriate properties.  It also creates:

- TPersonList class based on TtiFilteredObjectList and
  registers the object and its properties with the tiAutoMap feature of tiOPF.

- Creates hard coded visitors for Read, Create, Update and Delete for both TPerson
as well as separate visitors for TPersonList.

- Introduces a .FindByOID method to each object list created such as our TPersonList
example so that you can do MyPersonList.FindByOID('123') and the objectlist will be
cleared and then populated with the matchin object if found.  It's a function that returns
the number of object returned.  The schema allows you to define if classes use string or
integer based OID and writes out the method signature accordingly (like adding quotes if
necessary).

- Writes specialized visitors and adds named methods to all automatically created
object lists as described in the schema.  Vis:

<selections>
    <select type="func" name="FindByFirstName">
        <params>
            <item name="AName" type="string" pass-by="const" sql-param="user_first"/>
        </params>
        <sql>
            <![CDATA[
                SELECT
                  ${field_list}
                FROM
                    PERSON
                WHERE
                    PERSON.FIRST_NAME = :USER_FIRST
            ]]>
        </sql>
    </select>
</selections>

The <class> tag of the schema can also contain a <selections> tag which holds one or
more <select> tags which describe a method that will be written into the auto
generated object list classes.  The utility actually creates an additional visitor specifically
for handling the method.  The <select> above would result in the following:

  TPersonList = class(TtiMappedFilteredObjectList)
  public
    // typical overriden methods for list here

    { Returns Number of objects retrieved. }
    function    FindByFirstName(const AName: string): integer;
  end;

  function TPersonList.FindByFirstName(const AName: string): integer;
  begin
    if self.Count > 0 then
      self.Clear;

    Params.Clear;
    AddParam('AName', 'user_first', ptString, AName);
    self.SQL :=
      ' SELECT PERSON.OID , PERSON.FIRST_NAME, PERSON.LAST_NAME,  ' +
      ' PERSON.AGE, PERSON.GENDER FROM PERSON WHERE PERSON.FIRST_NAME  ' +
      ' = :USER_FIRST';
    GTIOPFManager.VisitorManager.Execute('TPersonList_FindByFirstNameVis', self);
    result := self.Count;
  end;

Notice that the class definition for TPersonList has a built in method called
FindByFirstName which takes a const AName: string param and returns and integer
indicating the number of objects that got populated into the object list.  All of
the sql and code to use the sql is written automatically.

The utility also creates a specialized visitor and fleshes it out.

  TPersonList_FindByFirstNameVis = class(TtiMapParameterListReadVisitor)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   MapRowToObject; override;
  end;


Please see the /demos/bom/sample.xml file for a working example.







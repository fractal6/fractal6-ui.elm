module Components.Text exposing (..)

-- Operational


loading : String
loading =
    "loading..."


seeMore : String
seeMore =
    "See more"


welcomIn : String
welcomIn =
    "Welcome In"


checkItOut : String
checkItOut =
    "Check it out."


edit : String
edit =
    "Edit"


cancel : String
cancel =
    "Cancel"



-- Genral


by : String
by =
    "by"


leaveComment : String
leaveComment =
    "Leave a comment"



--Quick Search
--(header)


nameH : String
nameH =
    "Name"


circleH : String
circleH =
    "Circle"


parentH : String
parentH =
    "Parent"


firstLinkH : String
firstLinkH =
    "First Link"


phQS : String
phQS =
    "Find a Role or Circle"



--Canvas


reverseTooltip : String
reverseTooltip =
    "Reverse the organisation graph."



-- Mandate


aboutH : String
aboutH =
    "About"


mandateH : String
mandateH =
    "Mandate"


purposeH : String
purposeH =
    "Purpose"


responsabilitiesH : String
responsabilitiesH =
    "Responsibilities"


domainsH : String
domainsH =
    "Domains"


policiesH : String
policiesH =
    "Policies"


noResponsabilities : String
noResponsabilities =
    "no responsibilities provided"


noDomains : String
noDomains =
    "no domains provided"


noPolicies : String
noPolicies =
    "no policies provided"


phCirclePurpose : String
phCirclePurpose =
    "Define the purpose of the circle"


phCircleResponsabilities : String
phCircleResponsabilities =
    "Define the circle responsibilities"


phCircleDomains : String
phCircleDomains =
    "Define the circle domains"


phCirclePolicies : String
phCirclePolicies =
    "Define the circle policies"


phRolePurpose : String
phRolePurpose =
    "Define the purpose of the role"


phRoleResponsabilities : String
phRoleResponsabilities =
    "Define the role responsibilities"


phRoleDomains : String
phRoleDomains =
    "Define the role domains"


phRolePolicies : String
phRolePolicies =
    "Define the role policies"



-- Tension


tensionH : String
tensionH =
    "Tensions"


journalH : String
journalH =
    "Journal"


noTensionRole : String
noTensionRole =
    "No tensions for this Role yet."


noTensionCircle : String
noTensionCircle =
    "No tensions for this Circle yet."


internalTensions =
    "Internal Tensions"


externalTensions =
    "External Tensions"


noIntTensionRole : String
noIntTensionRole =
    "No internal tensions for this Role yet."


noIntTensionCircle : String
noIntTensionCircle =
    "No internal tensions for this Circle yet."


noExtTensionRole : String
noExtTensionRole =
    "No external tensions for this Role yet."


noExtTensionCircle : String
noExtTensionCircle =
    "No external tensions for this Circle yet."


newTension : String
newTension =
    "new Tension"


newCircle : String
newCircle =
    "New Circle"


newRole : String
newRole =
    "New Role"


editCircle : String
editCircle =
    "Edit Circle"


editRole : String
editRole =
    "Edit Role"


updateAbout : String
updateAbout =
    "Edit about"


updateMandate : String
updateMandate =
    "Edit mandate"


tensionTitleHelp : String
tensionTitleHelp =
    "Title that sumarize your tension."


circleNameHelp : String
circleNameHelp =
    "Name of the circle."


roleNameHelp : String
roleNameHelp =
    "Name of the role."


roleAboutHelp : String
roleAboutHelp =
    "Short description of this role."


circleAboutHelp : String
circleAboutHelp =
    "Short description of this circle."


tensionMessageHelp : String
tensionMessageHelp =
    "Add a description to help others understand your issue."


circleMessageHelp : String
circleMessageHelp =
    "Add a description to help others understand why a new circle should be created."


roleMessageHelp : String
roleMessageHelp =
    "Add a description to help others understand why a new role should be created."


autoFieldMessageHelp : String
autoFieldMessageHelp =
    "These fields are use for ressource identification."


firstLinkRoleMessageHelp : String
firstLinkRoleMessageHelp =
    "Select a role type and assign user (first link)."


firstLinkCircleMessageHelp : String
firstLinkCircleMessageHelp =
    "Assign an user to a coordinator Role (first link)."


tensionAdded : String
tensionAdded =
    "Tension added."


roleAdded : String
roleAdded =
    "Role added."


circleAdded : String
circleAdded =
    "Circle added."


roleEdited : String
roleEdited =
    "Role edited."


circleEdited : String
circleEdited =
    "Circle edited."


tensionCircleAdded : String
tensionCircleAdded =
    "Tension added (new Circle)."


tensionRoleAdded : String
tensionRoleAdded =
    "Tension added (new Role)."


tensionRoleEdit : String
tensionRoleEdit =
    "Tension added (Role edit)."


tensionCircleEdit : String
tensionCircleEdit =
    "Tension added (Circle edit)."


tensionSubmit : String
tensionSubmit =
    "Submit tension"


tensionCircleSubmit : String
tensionCircleSubmit =
    tensionSubmit


tensionRoleSubmit : String
tensionRoleSubmit =
    tensionSubmit


editAndClose : String
editAndClose =
    "Edit and close tension."


tensionCircleCloseSubmit : String
tensionCircleCloseSubmit =
    "Create Circle and close tension"


tensionRoleCloseSubmit : String
tensionRoleCloseSubmit =
    "Create Role and close tension"


openedThe : String
openedThe =
    "opened the"


commentedThe : String
commentedThe =
    "commented the"


updateTitle : String
updateTitle =
    "Update title"


updateComment : String
updateComment =
    "Update comment"



-- Action
--@Todo
-- Organisation


notOrgMember : String
notOrgMember =
    "You are not a member of this organisation."


notCircleMember : String
notCircleMember =
    "You are not a member of this circle."


notCircleCoordo : String
notCircleCoordo =
    "You are not a coordinator of this circle."


askCoordo : String
askCoordo =
    "Please, ask a coordinator of this circle to perform this action."


joinForTension : String
joinForTension =
    "Please, Join this organisation to be able to create a tension."


joinForCircle : String
joinForCircle =
    "Please, Join this organisation to be able to create a circle."


nodeNotExist : String
nodeNotExist =
    "Sorry, this node doesn't exist yet."

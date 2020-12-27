module Text exposing (..)

-- General


signin : String
signin =
    "Login"


signout : String
signout =
    "Sign Out"


createAccount : String
createAccount =
    "create a new account"


createLabel : String
createLabel =
    "Create label"


updateLabel : String
updateLabel =
    "Update label"


messageSent : String
messageSent =
    "Message sent"


profile : String
profile =
    "Profile"


settings : String
settings =
    "Settings"


yourOrg : String
yourOrg =
    "Organisations"


loading : String
loading =
    "loading..."


seeMore : String
seeMore =
    "See more"


welcomIn : String
welcomIn =
    "Welcome In"


organisations : String
organisations =
    "organisations"


joinOrga : String
joinOrga =
    "Join this organisation"


leaveRole : String
leaveRole =
    "Leave role"


checkItOut : String
checkItOut =
    "Check it out."


explore : String
explore =
    "Explore"


exploreOrganisations : String
exploreOrganisations =
    "Explore organisations"


newOrganisation : String
newOrganisation =
    "New organisation"


view : String
view =
    "View"


versions : String
versions =
    "Versions"


revisions : String
revisions =
    "Revisions"


history : String
history =
    "History"


edit : String
edit =
    "Edit"


cancel : String
cancel =
    "Cancel"


publish : String
publish =
    "publish"


publishedThe : String
publishedThe =
    "Published the"


by : String
by =
    "by"


join : String
join =
    "join"


left : String
left =
    "left"


saveChanges : String
saveChanges =
    "Save changes"


leaveComment : String
leaveComment =
    "Leave a comment"


noResultsFound : String
noResultsFound =
    "No results found"


searchUsers : String
searchUsers =
    "Search users"


reopened : String
reopened =
    "reopened"


closed : String
closed =
    "closed"


assigned : String
assigned =
    "assigned"


unassigned : String
unassigned =
    "unassigned"


published : String
published =
    "published"


archive : String
archive =
    "Archive"


unarchive : String
unarchive =
    "Unarchive"


archived : String
archived =
    "archived"


unarchived : String
unarchived =
    "unarchived"


leave : String
leave =
    "Leave"


the : String
the =
    "the"


updatedTitle : String
updatedTitle =
    "updated title"



--Quick Search
--(header)


nameH : String
nameH =
    "Name"


circleH : String
circleH =
    "Circle"


roleH : String
roleH =
    "Role"


parentH : String
parentH =
    "Parent"


firstLinkH : String
firstLinkH =
    "First Link"


phQS : String
phQS =
    "Find a Role or Circle"


documentH : String
documentH =
    "Document"



--Canvas


reverseTooltip : String
reverseTooltip =
    "Reverse the organisation graph."



-- Mandate


aboutH : String
aboutH =
    "About"


linksH : String
linksH =
    "Links"


mandateH : String
mandateH =
    "Mandate"


purposeH : String
purposeH =
    "Purpose"


responsabilitiesH : String
responsabilitiesH =
    "Accountabilities"


domainsH : String
domainsH =
    "Domains"


policiesH : String
policiesH =
    "Policies"


createH : String
createH =
    "Create"


noResponsabilities : String
noResponsabilities =
    "no accountabilities provided"


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
    "Define the circle accountabilities"


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
    "Define the role accountabilities"


phRoleDomains : String
phRoleDomains =
    "Define the role domains"


phRolePolicies : String
phRolePolicies =
    "Define the role policies"


noFirstLinksRole : String
noFirstLinksRole =
    "No members are linked to this role yet."


noFirstLinksCircle : String
noFirstLinksCircle =
    "No members are linked to this circle yet."



-- Tension


tensionH : String
tensionH =
    "Tensions"


journalH : String
journalH =
    "Journal"


labelsH : String
labelsH =
    "Labels"


assigneesH : String
assigneesH =
    "Assignees"


actionH : String
actionH =
    "Action"


noAssignees : String
noAssignees =
    "No assignees"


noLabels : String
noLabels =
    "None yet"


noAction : String
noAction =
    "No action requested"


noOpenTensionRole : String
noOpenTensionRole =
    "No open tensions for this Role yet."


noOpenTensionCircle : String
noOpenTensionCircle =
    "No open tensions for this Circle yet."


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
    "New Tension"


newCircle : String
newCircle =
    "New Circle"


newRole : String
newRole =
    "New Role"


editTitle : String
editTitle =
    "Edit title"


tensionTitleHelp : String
tensionTitleHelp =
    "Title that sumarize your tension."


circleNameHelp : String
circleNameHelp =
    "Name of the circle."


roleNameHelp : String
roleNameHelp =
    "Name of the role."


orgaNameHelp : String
orgaNameHelp =
    "Organisation name"


aboutHelp : String
aboutHelp =
    "Short description for this organisation"


purposeHelpOrga : String
purposeHelpOrga =
    "Purpose of this organisation"


roleAboutHelp : String
roleAboutHelp =
    "Short description for this role."


circleAboutHelp : String
circleAboutHelp =
    "Short description for this circle."


tensionMessageHelp : String
tensionMessageHelp =
    "Add a comment to help others understand your issue."


actionMessageHelp : String
actionMessageHelp =
    "Add a comment to help others understand this action."


circleMessageHelp : String
circleMessageHelp =
    "Add a comment to help others understand why a new circle should be created."


roleMessageHelp : String
roleMessageHelp =
    "Add a comment to help others understand why a new role should be created."


autoFieldMessageHelp : String
autoFieldMessageHelp =
    "URL unique identifier"


firstLinkRoleMessageHelp : String
firstLinkRoleMessageHelp =
    "Select a type of role and assign an user (first link)."


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
    "Tension added for Circle (not published)."


tensionRoleAdded : String
tensionRoleAdded =
    "Tension added for Role (not published)."


tensionSubmit : String
tensionSubmit =
    "Submit tension"


tensionCircleCloseSubmit : String
tensionCircleCloseSubmit =
    "Create Circle"


tensionRoleCloseSubmit : String
tensionRoleCloseSubmit =
    "Create Role"


openedThe : String
openedThe =
    "opened the"


updatedThe : String
updatedThe =
    "updated the"


editedThe : String
editedThe =
    "edited the"


commentedThe : String
commentedThe =
    "commented the"


updateTitle : String
updateTitle =
    "Update title"


updateComment : String
updateComment =
    "Update comment"


addResponsabilities : String
addResponsabilities =
    "Add accountabilities"


addDomains : String
addDomains =
    "Add domains"


addPolicies : String
addPolicies =
    "Add policies"



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


joinForComment : String
joinForComment =
    "Please, Join this organisation to participate to this conversation."


joinForCircle : String
joinForCircle =
    "Please, Join this organisation to be able to create a circle."


nodeNotExist : String
nodeNotExist =
    "Sorry, this node doesn't exist yet."

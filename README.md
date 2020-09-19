
# File Structure

# Components

For complex component that cary their own state, the code is organised as follows:

### State Data
The state of a component is carry by some data that are own by the component, meaning that they shouldnt be accessed from another scope than the file where the compponent is defined (note this rule can be bypassed in the case where it allow too keep the code simpler or if there is an emergency. An exmple of such a bypass exists in the update of the `form` variable in the `DoTensionSource` and `DoCircleSource` Msg. Usually, the record type that carry the data is name as the elm file of the component.

### Op Data

They contains the message available in the component and the data that that are not own by the component but that affect its state.


### Methods

The methods of the component are used to update the component state. They should take the state data object as parameter and return it updated for an outside usage.




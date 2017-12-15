
# Tests:

The following tests are made up of programs that are contained in the 'tests' folder.

For each test only the first instruction is run, to see if the desired effect was achieved.

### Test ADD

"Takes the contents of the A-Field and adds it to the contents of the B-Field of the instruction that the B-Field is pointing at."

Instruction|Desired Effect
---|---
`1.ADD 1, 1`<br>`2.MOV 0, #0`|`1.ADD 1, 1`<br>`2.MOV 0, #1`

### Test DAT

"Causes a program to halt when it is executed."

Instruction|Desired Effect
---|---
`1.DAT 0`|Task is killed

### Test MOV with an immediate A-Field

"Creates a DAT instruction in the instruction pointed at by the B-Field and the value of the A-field is placed in the new instructions B-field."

Instruction|Desired Effect
---|---
`1.MOV #1, 1`<br>`2.MOV 0, #1`|`1.MOV #1, 1`<br>`2.DAT 1`

### Test MOV without an immediate A-Field

"Copies the complete contents of the location indicated by the A field into the location indicated by the B field."

Instruction|Desired Effect
---|---
`1.MOV 0, 1`<br>`2.DAT 0`|`1.MOV #0, 1`<br>`2.MOV #0, 1`

### Test DJN when not caused to jump

"Decrements the value indicated by the B-Field and then jumps to the instruction indicated by the A-Field if the number indicated by the B-Field has become non-zero."

Instruction|Desired Effect
---|---
`1.DJN 2, 0`<br>`2.DAT 0`<br>`3.MOV 0, #1`|The next instruction executed is instruction 2.

### Test DJN when caused to jump

"Decrements the value indicated by the B-Field and then jumps to the instruction indicated by the A-Field if the number indicated by the B-Field has become non-zero."

Instruction|Desired Effect
---|---
`1.DJN 2, 1`<br>`2.DAT 0`<br>`3.MOV 0, #1`|The next instruction executed is instruction 1.

### Test CMP with equal values in the A-Field and B-Field

"Compares the values indicated by the A and B-Fields and skips the next instruction if they are not equal."

Instruction|Desired Effect
---|---
`1.CMP 0, 0`<br>`2.DAT 0`<br>`3.MOV 0, #1`|The next instruction executed is instruction 2.

### Test CMP without equal values in the A-Field and B-Field

"Compares the values indicated by the A and B-Fields and skips the next instruction if they are not equal."

Instruction|Desired Effect
---|---
`1.CMP 0, 1`<br>`2.DAT 0`<br>`3.MOV 0, #1`|The next instruction executed is instruction 3.

### Test SPL

"Splits execution into the next instruction and the instruction contained in the specified address."

Instruction|Desired Effect
---|---
`1.SPL 2`<br>`2.DAT 0`<br>`3.MOV 0, #1`|The warrior is split to have another task. The next instruction executed is instruction 3 and then instruction 2.

### Test JMZ with zero value in the B-Field

"Jumps to the instruction indicated by the A-Field if the value indicated by the B-Field is zero, otherwise it does nothing."

Instruction|Desired Effect
---|---
`1.JMZ 2, 0`<br>`2.DAT 0`<br>`3.MOV 0, #0`|The next instruction executed is instruction 3.

### Test JMZ without zero value in the B-Field

"Jumps to the instruction indicated by the A-Field if the value indicated by the B-Field is zero, otherwise it does nothing."

Instruction|Desired Effect
---|---
`1.JMZ 2, 1`<br>`2.DAT 0`<br>`3.MOV 0, #0`|The next instruction executed is instruction 2.

### Test SUB

"Takes the contents of the A-Field and subtracts it from the contents of the B-Field of the instruction that the B-Field is pointing at."

Instruction|Desired Effect
---|---
`1.SUB 1, 1`<br>`2.MOV 0, #0`|`1.SUB 1, 1`<br>`2.MOV 0, #-1`

### Test JMP

"Causes the instruction referenced in the A-Field to be the next one executed."

Instruction|Desired Effect
---|---
`1.JMP 2`<br>`2.MOV 0, #0`<br>`3.DAT 0`|The next instruction executed is instruction 3.

### Test JMN with zero valued B-Field

"Jumps to the instruction indicated by the A-Field if the value indicated by the B-Field is non-zero, otherwise it does nothing."

Instruction|Desired Effect
---|---
`1.JMN 2, 0`<br>`2.DAT 0`<br>`3.MOV 0, #1`|The next instruction executed is instruction 2.

### Test JMN with non-zero valued B-Field

"Jumps to the instruction indicated by the A-Field if the value indicated by the B-Field is non-zero, otherwise it does nothing."

Instruction|Desired Effect
---|---
`1.JMN 2, 1`<br>`2.DAT 0`<br>`3.MOV 0, #1`|The next instruction executed is instruction 3.

### Test that Direct addressing works

"Direct (no prefix, or a dollar sign), means the address itself is the target."

Instruction|Desired Effect
---|---
`1.ADD #1, 2`<br>`2.MOV 0, #1`<br>`3.MOV 0, #1`|`1.ADD #1, 2`<br>`2.MOV 0, #1`<br>`3.MOV 0, #2`

### Test that Indirect addressing works

"Indirect (prefixed by @), means that the contents of the address specify the target address"

Instruction|Desired Effect
---|---
`1.ADD #1, @2`<br>`2.DAT 0`<br>`3.MOV 0, #1`<br>`4.MOV 0, #1`|`1.ADD #1, @2`<br>`2.DAT 0`<br>`3.MOV 0, #1`<br>`4.MOV 0, #2`

### Test that Immediate addressing works

"Immediate (prefixed by #), which means that the address should be treated as an integer"

Instruction|Desired Effect
---|---
`1.ADD #1, #2`<br>`2.DAT 0`<br>`3.MOV 0, #1`|`1.ADD #1, #2`<br>`2.DAT 0`<br>`3.MOV 0, #2`

### Test that AutoDecrement addressing works

"Auto-decrement (prefixed by <) <del>which indicates that the number in the field should be decremented by one and then treated as an indirect address</del> should decrement the value it points at rather than the value it directly contains. "

Instruction|Desired Effect
---|---
`1.ADD #1, <2`<br>`2.DAT 0`<br>`3.MOV 0, #2`<br>`4.MOV 0, #2`|`1.ADD #1, <2`<br>`2.DAT 0`<br>`3.MOV 0, #2`<br>`4.MOV 0, #3`

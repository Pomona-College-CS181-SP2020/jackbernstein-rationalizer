
# 4-2-2020 BUG 1

Problem: Filling in text to the one box fills the content of another box

Fix: Was calling the wrong Msg, probably came from using copy and paste

# 4-2-2020 BUG 2

Problem: Once numbers are input for quantity, can't delete the numbers to get back to 0

Fix: Added case for empty string

# 4-7-2020 BUG 3

Problem: Cannot input decimal numbers

Fix: Changed quantity to a string

# 4-7-2020 BUG 4

Problem: Can submit recipe even when ingredients/quantity left empty

Fix: Checks each text box before submitting

# 4-8-2020 BUG 5

Problem: Does not check if quantity is incomplete float before submitting

Fix: Changed boolean logic to string logic

# 4-9-2020

Problem: Build fails while trying to install elm

Fix: Switched to Elm 0.19.0, Node 9

# 4-13-2020

Problem: Can't go back to 0 when rationalizing

Fix: Allow user to put in any input

# 4-24-2020

Problem: Input won't reset to 0 when select is changed 

Fix: Didn't need to fix because removed menu

# 4-24-2020

Problem: Drop down menu is not calling methods in update

Fix: Moved method call from option to select

# 4-24-2020

Problem: Bugs when selecting ingredient to rationalize around

Fix: Changed overall logic, bug disappeared 




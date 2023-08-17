package com.example.myapp.mainActivities.account;

import android.annotation.SuppressLint;
import android.graphics.Paint;
import android.os.Bundle;
import android.text.Editable;
import android.text.InputFilter;
import android.text.TextWatcher;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.user.User;
import com.google.android.material.textfield.TextInputLayout;

import java.util.HashMap;
import java.util.Map;

public class AccountActivity extends AppCompatActivity {

    Map<LinearLayout, Boolean> linearLayoutBooleanMap = new HashMap<>();

    AccountViewModel accountViewModel;

    LinearLayout layoutCreationVisible, layoutUsernameVisible, layoutPasswordVisible, layoutDeletionVisible;
    LinearLayout layoutCreationHidden, layoutUsernameHidden, layoutPasswordHidden, layoutDeletionHidden;

    TextView accountCreationTitle, changeUsernameTitle, changePasswordTitle, accountDeletionTitle;

    Button loginButton, newUserButton, changeUsernameButton, changePasswordButton, deleteUserButton;

    TextInputLayout newUsernameInput, newPasswordInput, newPasswordConfirmInput;
    TextInputLayout changeUsernameInput, changePasswordInput, changePasswordConfirmInput;
    TextInputLayout deletePasswordInput, deletePasswordConfirmInput;

    EditText newUsername, newPassword, newPasswordConfirm;
    EditText changeUsername, changePassword, changePasswordConfirm;
    EditText deletePassword, deletePasswordConfirm;

    TextView titleText;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_account);
        //get view model
        accountViewModel = new ViewModelProvider(this).get(AccountViewModel.class);
        //load user with given username
        accountViewModel.loadUser(getIntent().getExtras().getString("username"));
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //link all components with ID
        initialiseViewByID();
        //initialise visible and hidden layouts
        initialiseLinearLayouts();
        //initialise edit text for user input
        initialiseTextInputs();
        //initialise all buttons
        initialiseButtons();
        //reload textview and layouts
        reloadPage();
    }

    //link all components with ID
    public void initialiseViewByID(){
        //initialise username text view on top
        titleText = findViewById(R.id.titleText);
        //initialise login button
        loginButton = findViewById(R.id.loginButton);
        //initialise user creation layout components by ID
        initialiseCreationView();
        //initialise change username layout components by ID
        initialiseUsernameView();
        //initialise change password layout components by ID
        initialisePasswordView();
        //initialise user deletion layout components by ID
        initialiseDeletionView();
    }

    //initialise user creation layout components by ID
    public void initialiseCreationView(){
        layoutCreationVisible = findViewById(R.id.layoutCreationVisible);
        layoutCreationHidden = findViewById(R.id.layoutCreationHidden);
        accountCreationTitle = findViewById(R.id.accountCreationTitle);
        newUsernameInput = findViewById(R.id.newUsernameInput);
        newPasswordInput = findViewById(R.id.newPasswordInput);
        newPasswordConfirmInput = findViewById(R.id.newPasswordConfirmInput);
        newUsername = findViewById(R.id.newUsername);
        newPassword = findViewById(R.id.newPassword);
        newPasswordConfirm = findViewById(R.id.newPasswordConfirm);
        newUserButton = findViewById(R.id.newUserButton);
    }

    //initialise change username layout components by ID
    public void initialiseUsernameView(){
        layoutUsernameVisible = findViewById(R.id.layoutUsernameVisible);
        layoutUsernameHidden = findViewById(R.id.layoutUsernameHidden);
        changeUsernameTitle = findViewById(R.id.changeUsernameTitle);
        changeUsernameInput = findViewById(R.id.changeUsernameInput);
        changeUsername = findViewById(R.id.changeUsername);
        changeUsernameButton = findViewById(R.id.changeUsernameButton);
    }

    //initialise change password layout components by ID
    public void initialisePasswordView(){
        layoutPasswordVisible = findViewById(R.id.layoutPasswordVisible);
        layoutPasswordHidden = findViewById(R.id.layoutPasswordHidden);
        changePasswordTitle = findViewById(R.id.changePasswordTitle);
        changePasswordInput = findViewById(R.id.changePasswordInput);
        changePasswordConfirmInput = findViewById(R.id.changePasswordConfirmInput);
        changePassword = findViewById(R.id.changePassword);
        changePasswordConfirm = findViewById(R.id.changePasswordConfirm);
        changePasswordButton = findViewById(R.id.changePasswordButton);
    }

    //initialise user deletion layout components by ID
    public void initialiseDeletionView(){
        layoutDeletionVisible = findViewById(R.id.layoutDeletionVisible);
        layoutDeletionHidden = findViewById(R.id.layoutDeletionHidden);
        accountDeletionTitle = findViewById(R.id.accountDeletionTitle);
        deletePasswordInput = findViewById(R.id.deletePasswordInput);
        deletePasswordConfirmInput = findViewById(R.id.deletePasswordConfirmInput);
        deletePassword = findViewById(R.id.deletePassword);
        deletePasswordConfirm = findViewById(R.id.deletePasswordConfirm);
        deleteUserButton = findViewById(R.id.deleteUserButton);
    }

    //setup all visible and hidden layouts
    public void initialiseLinearLayouts(){
        setupLayouts(layoutCreationVisible, layoutCreationHidden);
        setupLayouts(layoutUsernameVisible, layoutUsernameHidden);
        setupLayouts(layoutPasswordVisible, layoutPasswordHidden);
        setupLayouts(layoutDeletionVisible, layoutDeletionHidden);
    }

    //setup visible and hidden layout
    public void setupLayouts(LinearLayout layoutVisible, LinearLayout layoutHidden){
        //hide hidden layout
        layoutHidden.setVisibility(View.GONE);
        //add hidden layout to hashmap to record visibility
        linearLayoutBooleanMap.put(layoutHidden, false);
        //add on click listener to visible layout
        layoutVisible.setOnClickListener(view -> {
            //invert hidden layout visibility when visible layout clicked
            linearLayoutBooleanMap.put(layoutHidden, Boolean.FALSE.equals(linearLayoutBooleanMap.get(layoutHidden)));
            layoutHidden.setVisibility(Boolean.TRUE.equals(linearLayoutBooleanMap.get(layoutHidden)) ? View.VISIBLE : View.GONE);
        });
    }

    //initialise edit text for user input
    public void initialiseTextInputs(){
        //initialise user creation edit text
        initialiseNewUser();
        //initialise change username edit text
        initialiseChangeUsername();
        //initialise change password edit text
        initialiseChangePassword();
        //initialise user deletion edit text
        initialiseDeleteUser();
    }

    //initialise user creation edit text
    public void initialiseNewUser(){
        //force uppercase
        newUsername.setFilters(new InputFilter[] {new InputFilter.AllCaps()});
        //add text watcher and on focus listener to new username edit text
        newUsername.addTextChangedListener(newUserTextWatcher);
        newUsername.setOnFocusChangeListener((v, hasFocus) -> validateUsername(newUsernameInput, newUsername));
        //add text watcher and on focus listener to new password edit text
        newPassword.addTextChangedListener(newUserTextWatcher);
        newPassword.setOnFocusChangeListener((v, hasFocus) -> validatePassword(newPasswordInput, newPassword, newPasswordConfirm, null));
        //add text watcher and on focus listener to new password confirm edit text
        newPasswordConfirm.addTextChangedListener(newUserTextWatcher);
        newPasswordConfirm.setOnFocusChangeListener((v, hasFocus) -> validatePassword(newPasswordConfirmInput, newPasswordConfirm, newPassword, null));
    }

    //initialise change username edit text
    public void initialiseChangeUsername(){
        //force uppercase
        changeUsername.setFilters(new InputFilter[] {new InputFilter.AllCaps()});
        //add text watcher and on focus listener to change username edit text
        changeUsername.addTextChangedListener(changeUsernameTextWatcher);
        changeUsername.setOnFocusChangeListener((v, hasFocus) -> validateUsername(changeUsernameInput, changeUsername));
    }

    //initialise change password edit text
    public void initialiseChangePassword(){
        //add text watcher and on focus listener to change password edit text
        changePassword.addTextChangedListener(changePasswordTextWatcher);
        changePassword.setOnFocusChangeListener((v, hasFocus) -> validatePassword(changePasswordInput, changePassword, changePasswordConfirm, false));
        //add text watcher and on focus listener to change password confirm edit text
        changePasswordConfirm.addTextChangedListener(changePasswordTextWatcher);
        changePasswordConfirm.setOnFocusChangeListener((v, hasFocus) -> validatePassword(changePasswordConfirmInput, changePasswordConfirm, changePassword, false));
    }

    //initialise user deletion edit text
    public void initialiseDeleteUser(){
        //add text watcher and on focus listener to delete password edit text
        deletePassword.addTextChangedListener(deleteUserTextWatcher);
        deletePassword.setOnFocusChangeListener((v, hasFocus) -> validatePassword(deletePasswordInput, deletePassword, deletePasswordConfirm, true));
        //add text watcher and on focus listener to delete password confirm edit text
        deletePasswordConfirm.addTextChangedListener(deleteUserTextWatcher);
        deletePasswordConfirm.setOnFocusChangeListener((v, hasFocus) -> validatePassword(deletePasswordConfirmInput, deletePasswordConfirm, deletePassword, true));
    }

    //initialise all buttons
    public void initialiseButtons(){
        //initialise create user button
        initialiseCreateButton();
        //initialise change username button
        initialiseChangeUsernameButton();
        //initialise change password button
        initialiseChangePasswordButton();
        //initialise delete user button
        initialiseDeleteButton();
        //initialise login button
        initialiseLoginButton();
    }

    //initialise create user button
    public void initialiseCreateButton(){
        //create user button on click listener
        newUserButton.setOnClickListener(v -> {
            //get new username
            String usernameText = newUsername.getText().toString();
            //get new password
            String passwordText = newPassword.getText().toString();
            //create new user
            accountViewModel.createUser(usernameText, passwordText);
            //reload textview and layouts
            reloadPage();
        });
    }

    //initialise change username button
    public void initialiseChangeUsernameButton(){
        //change username button on click listener
        changeUsernameButton.setOnClickListener(v -> {
            //get new username
            String usernameText = changeUsername.getText().toString();
            //change username
            accountViewModel.changeUsername(usernameText);
            //reload textview and layouts
            reloadPage();
        });
    }

    //initialise change password button
    public void initialiseChangePasswordButton(){
        //change password button on click listener
        changePasswordButton.setOnClickListener(v -> {
            //get new password
            String passwordText = changePassword.getText().toString();
            //change password
            accountViewModel.changePassword(passwordText);
            //reload textview and layouts
            reloadPage();
        });
    }

    //initialise delete user button
    public void initialiseDeleteButton(){
        //delete user validation dialog on click
        deleteUserButton.setOnClickListener(view -> new AlertDialog.Builder(this)
                .setTitle("Account Deletion")
                .setMessage("Are you sure you want to delete your account? There is no way to recover your account once deleted.")
                .setPositiveButton("Yes", (dialog, which) -> {
                    //delete user
                    accountViewModel.deleteUser();
                    //reload textview and layouts
                    reloadPage();
                })
                .setNegativeButton("No", null)
                .create()
                .show());
    }

    //initialise login button
    public void initialiseLoginButton(){
        //send user to next activity on click
        loginButton.setOnClickListener(v -> {
            reloadPage();
            startActivity(accountViewModel.loginUser());
        });
    }

    //reload textview and layouts
    @SuppressLint("SetTextI18n")
    public void reloadPage(){
        //get current user
        User user = accountViewModel.getUser();
        //set username text view with username
        titleText.setText("Welcome " + user.getUsername());
        //enable login button if not new user
        loginButton.setEnabled(user.getUserID() >= 0);
        //clear all text fields
        clearTextFields();
        //clear all focus from all text fields
        clearFocus();
        //hide all hidden layouts
        hideLayouts(user.getUserID());
    }

    //hide all hidden layouts
    public void hideLayouts(int userID){
        hideLayout(layoutCreationVisible, layoutCreationHidden, accountCreationTitle, userID < 0);
        hideLayout(layoutUsernameVisible, layoutUsernameHidden, changeUsernameTitle, userID > 0);
        hideLayout(layoutPasswordVisible, layoutPasswordHidden, changePasswordTitle, userID > 0);
        hideLayout(layoutDeletionVisible, layoutDeletionHidden, accountDeletionTitle, userID > 0);
    }

    //hide hidden layout
    public void hideLayout(LinearLayout layoutVisible, LinearLayout layoutHidden, TextView title, boolean clickable){
        layoutHidden.setVisibility(View.GONE);
        layoutVisible.setEnabled(clickable);
        int paintFlags = clickable ? title.getPaintFlags() & (~Paint.STRIKE_THRU_TEXT_FLAG) : title.getPaintFlags() | Paint.STRIKE_THRU_TEXT_FLAG;
        title.setPaintFlags(paintFlags);
    }

    //clear all text fields
    public void clearTextFields(){
        newUsername.getText().clear();
        newPassword.getText().clear();
        newPasswordConfirm.getText().clear();
        changeUsername.getText().clear();
        changePassword.getText().clear();
        changePasswordConfirm.getText().clear();
        deletePassword.getText().clear();
        deletePasswordConfirm.getText().clear();
    }

    //clear all focus from all text fields
    public void clearFocus(){
        newUsername.clearFocus();
        newPassword.clearFocus();
        newPasswordConfirm.clearFocus();
        changeUsername.clearFocus();
        changePassword.clearFocus();
        changePasswordConfirm.clearFocus();
        deletePassword.clearFocus();
        deletePasswordConfirm.clearFocus();
    }

    //validate username
    public boolean validateUsername(TextInputLayout textInputLayout, EditText editText){
        //get username from edit text
        String usernameText = editText.getText().toString();
        //check if edit text has focus
        boolean hasFocus = editText.hasFocus();
        //check if edit text is empty
        boolean emptyUsername = usernameText.isEmpty();
        //check if username taken
        boolean validUsername = !emptyUsername && accountViewModel.validateUsername(usernameText);

        if(!hasFocus || validUsername) //if edit text has no focus or valid username
            textInputLayout.setErrorEnabled(false); //show no error
        else if(emptyUsername) //if empty username
            textInputLayout.setError("Username cannot be empty"); //show error
        else //if username taken
            textInputLayout.setError("Username already taken"); //show error
        return validUsername; //return validity of username
    }

    //validate password
    public boolean validatePassword(TextInputLayout textInputLayout, EditText editText1, EditText editText2, Boolean equal){
        //get old password of user
        String oldPassword = accountViewModel.getUser().getPassword();
        //get password from edit text
        String newPasswordText = editText1.getText().toString();
        //get password from second edit text
        String newPasswordConfirmText = editText2.getText().toString();

        //check if edit text has focus
        boolean hasFocus = editText1.hasFocus();
        //check if password is empty
        boolean emptyPassword = newPasswordText.isEmpty();
        //check if both password are same
        boolean validPassword = !emptyPassword && newPasswordText.equals(newPasswordConfirmText);
        //check if password is equal or different from old password
        boolean equalPassword = equal == null || equal == newPasswordText.equals(oldPassword);

        if(!hasFocus || (validPassword && equalPassword)) //if password has no focus or valid password
            textInputLayout.setErrorEnabled(false); //show no error
        else if(emptyPassword) //if empty password
            textInputLayout.setError("Password cannot be empty"); //show error
        else if(!validPassword) //if both passwords do not match
            textInputLayout.setError("Both passwords must match"); //show error
        else //if password is invalid
            textInputLayout.setError("Password must " + (equal ? "" : "not ") + "match old password"); //show error
        return validPassword && equalPassword; //return validity of password
    }

    //text watcher for new user edit text
    private final TextWatcher newUserTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            //check if username is valid
            boolean validUsername = validateUsername(newUsernameInput, newUsername);
            //check if password is valid
            boolean validPassword = validatePassword(newPasswordInput, newPassword, newPasswordConfirm, null);
            //check if password confirm is valid
            boolean validPasswordConfirm = validatePassword(newPasswordConfirmInput, newPasswordConfirm, newPassword, null);
            //enable new user button if username and passwords are valid
            newUserButton.setEnabled(validUsername && validPassword && validPasswordConfirm);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    //text watcher for change username edit text
    private final TextWatcher changeUsernameTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            //check if username is valid
            boolean validUsername = validateUsername(changeUsernameInput, changeUsername);
            //enable change username button if username valid
            changeUsernameButton.setEnabled(validUsername);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    //text watcher for change password edit text
    private final TextWatcher changePasswordTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            //check if password is valid
            boolean validPassword = validatePassword(changePasswordInput, changePassword, changePasswordConfirm, false);
            //check if password confirm is valid
            boolean validPasswordConfirm = validatePassword(changePasswordConfirmInput, changePasswordConfirm, changePassword, false);
            //enable change password button if passwords are valid
            changePasswordButton.setEnabled(validPassword && validPasswordConfirm);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    //text watcher for delete user edit text
    private final TextWatcher deleteUserTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            //check if password is valid
            boolean validPassword = validatePassword(deletePasswordInput, deletePassword, deletePasswordConfirm, true);
            //check if password confirm is valid
            boolean validPasswordConfirm = validatePassword(deletePasswordConfirmInput, deletePasswordConfirm, deletePassword, true);
            //enable delete user button if passwords are valid
            deleteUserButton.setEnabled(validPassword && validPasswordConfirm);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    @Override
    protected void onResume() {
        super.onResume();
        //reset music player and logs when page resumes
        accountViewModel.resetApp();
    }
}
package com.example.myapp.mainActivities;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.graphics.Paint;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.User;
import com.example.myapp.databaseFiles.viewModal.AccountViewModal;
import com.google.android.material.textfield.TextInputLayout;

import java.util.HashMap;
import java.util.Map;

public class Account extends AppCompatActivity {

    Map<LinearLayout, Boolean> linearLayoutBooleanMap = new HashMap<>();
    AccountViewModal accountViewModal;

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

        accountViewModal = new ViewModelProvider(this).get(AccountViewModal.class);
        loadUserData(getIntent().getExtras());
        initialiseAll();
        reloadPage();
    }

    public void loadUserData(Bundle extras){
        accountViewModal.loadUser(extras.getInt("userID"), extras.getString("username"), extras.getString("password"));
    }

    public void initialiseAll(){
        initialiseTextViews();
        initialiseLinearLayouts();
        initialiseTextInputLayouts();
        initialiseEditTexts();
        initialiseButtons();
        initialiseTextInputs();
    }

    public void initialiseTextViews(){
        titleText = findViewById(R.id.titleText);
        accountCreationTitle = findViewById(R.id.accountCreationTitle);
        changeUsernameTitle = findViewById(R.id.changeUsernameTitle);
        changePasswordTitle = findViewById(R.id.changePasswordTitle);
        accountDeletionTitle = findViewById(R.id.accountDeletionTitle);
    }

    public void initialiseLinearLayouts(){
        layoutCreationVisible = findViewById(R.id.layoutCreationVisible);
        layoutCreationHidden = findViewById(R.id.layoutCreationHidden);
        setupLayouts(layoutCreationVisible, layoutCreationHidden);

        layoutUsernameVisible = findViewById(R.id.layoutUsernameVisible);
        layoutUsernameHidden = findViewById(R.id.layoutUsernameHidden);
        setupLayouts(layoutUsernameVisible, layoutUsernameHidden);

        layoutPasswordVisible = findViewById(R.id.layoutPasswordVisible);
        layoutPasswordHidden = findViewById(R.id.layoutPasswordHidden);
        setupLayouts(layoutPasswordVisible, layoutPasswordHidden);

        layoutDeletionVisible = findViewById(R.id.layoutDeletionVisible);
        layoutDeletionHidden = findViewById(R.id.layoutDeletionHidden);
        setupLayouts(layoutDeletionVisible, layoutDeletionHidden);
    }

    public void setupLayouts(LinearLayout layoutVisible, LinearLayout layoutHidden){
        layoutHidden.setVisibility(View.GONE);
        linearLayoutBooleanMap.put(layoutHidden, false);
        layoutVisible.setOnClickListener(view -> {
            linearLayoutBooleanMap.put(layoutHidden, Boolean.FALSE.equals(linearLayoutBooleanMap.get(layoutHidden)));
            layoutHidden.setVisibility(Boolean.TRUE.equals(linearLayoutBooleanMap.get(layoutHidden)) ? View.VISIBLE : View.GONE);
        });
    }

    public void initialiseTextInputLayouts(){
        newUsernameInput = findViewById(R.id.newUsernameInput);
        newPasswordInput = findViewById(R.id.newPasswordInput);
        newPasswordConfirmInput = findViewById(R.id.newPasswordConfirmInput);
        changeUsernameInput = findViewById(R.id.changeUsernameInput);
        changePasswordInput = findViewById(R.id.changePasswordInput);
        changePasswordConfirmInput = findViewById(R.id.changePasswordConfirmInput);
        deletePasswordInput = findViewById(R.id.deletePasswordInput);
        deletePasswordConfirmInput = findViewById(R.id.deletePasswordConfirmInput);
    }

    public void initialiseEditTexts(){
        newUsername = findViewById(R.id.newUsername);
        newPassword = findViewById(R.id.newPassword);
        newPasswordConfirm = findViewById(R.id.newPasswordConfirm);
        changeUsername = findViewById(R.id.changeUsername);
        changePassword = findViewById(R.id.changePassword);
        changePasswordConfirm = findViewById(R.id.changePasswordConfirm);
        deletePassword = findViewById(R.id.deletePassword);
        deletePasswordConfirm = findViewById(R.id.deletePasswordConfirm);
    }

    public void initialiseButtons(){
        initialiseCreateButton();
        initialiseChangeUsernameButton();
        initialiseChangePasswordButton();
        initialiseDeleteButton();
        initialiseLoginButton();
    }

    public void initialiseTextInputs(){
        initialiseNewUser();
        initialiseChangeUsername();
        initialiseChangePassword();
        initialiseDeleteUser();
    }

    @SuppressLint("SetTextI18n")
    public void reloadPage(){
        User user = accountViewModal.getUser();
        int userID = user.getUserID();
        titleText.setText("Welcome " + user.getUsername());
        loginButton.setEnabled(userID >= 0);
        clearTextFields();
        hideLayouts(userID);
    }

    public void hideLayouts(int userID){
        hideLayout(layoutCreationVisible, layoutCreationHidden, accountCreationTitle, userID < 0);
        hideLayout(layoutUsernameVisible, layoutUsernameHidden, changeUsernameTitle, userID >= 0);
        hideLayout(layoutPasswordVisible, layoutPasswordHidden, changePasswordTitle, userID >= 0);
        hideLayout(layoutDeletionVisible, layoutDeletionHidden, accountDeletionTitle, userID >= 0);
    }

    public void hideLayout(LinearLayout layoutVisible, LinearLayout layoutHidden, TextView title, boolean clickable){
        layoutHidden.setVisibility(View.GONE);
        layoutVisible.setEnabled(clickable);
        int paintFlags = clickable ? title.getPaintFlags() & (~Paint.STRIKE_THRU_TEXT_FLAG) : title.getPaintFlags() | Paint.STRIKE_THRU_TEXT_FLAG;
        title.setPaintFlags(paintFlags);
    }

    public void initialiseCreateButton(){
        newUserButton = findViewById(R.id.newUserButton);
        newUserButton.setOnClickListener(v -> {
            String usernameText = newUsername.getText().toString();
            String passwordText = newPassword.getText().toString();
            accountViewModal.insert(new User(usernameText, passwordText));
            Toast.makeText(getApplicationContext(), "New account created", Toast.LENGTH_SHORT).show();
            reloadPage();
        });
    }

    public void initialiseChangeUsernameButton(){
        changeUsernameButton = findViewById(R.id.changeUsernameButton);
        changeUsernameButton.setOnClickListener(v -> {
            String usernameText = changeUsername.getText().toString();
            accountViewModal.changeUsername(usernameText);
            Toast.makeText(getApplicationContext(), "Username changed", Toast.LENGTH_SHORT).show();
            reloadPage();
        });
    }

    public void initialiseChangePasswordButton(){
        changePasswordButton = findViewById(R.id.changePasswordButton);
        changePasswordButton.setOnClickListener(v -> {
            String passwordText = changePassword.getText().toString();
            accountViewModal.changePassword(passwordText);
            Toast.makeText(getApplicationContext(), "Password changed", Toast.LENGTH_SHORT).show();
            reloadPage();
        });
    }

    public void initialiseDeleteButton(){
        deleteUserButton = findViewById(R.id.deleteUserButton);
        deleteUserButton.setOnClickListener(view -> new AlertDialog.Builder(this)
                .setTitle("Account Deletion")
                .setMessage("Are you sure you want to delete your account? There is no way to recover your account once deleted.")
                .setPositiveButton("Yes", (dialog, which) -> {
                    accountViewModal.delete();
                    Toast.makeText(getApplicationContext(), "Account deleted", Toast.LENGTH_SHORT).show();
                    reloadPage();
                })
                .setNegativeButton("No", null)
                .create()
                .show());
    }

    public void initialiseLoginButton(){
        loginButton = findViewById(R.id.loginButton);
        loginButton.setOnClickListener(v -> {
            startActivity(new Intent(getApplicationContext(), Music.class));
            Toast.makeText(getApplicationContext(), "Welcome " + accountViewModal.getUser().getUsername(), Toast.LENGTH_SHORT).show();
        });
    }

    public void initialiseNewUser(){
        newUsername.addTextChangedListener(newUserTextWatcher);
        newUsername.setOnFocusChangeListener((v, hasFocus) -> validateUsername(newUsernameInput, newUsername));

        newPassword.addTextChangedListener(newUserTextWatcher);
        newPassword.setOnFocusChangeListener((v, hasFocus) -> validatePassword(newPasswordInput, newPassword, newPasswordConfirm, null));

        newPasswordConfirm.addTextChangedListener(newUserTextWatcher);
        newPasswordConfirm.setOnFocusChangeListener((v, hasFocus) -> validatePassword(newPasswordConfirmInput, newPasswordConfirm, newPassword, null));
    }

    public void initialiseChangeUsername(){
        changeUsername.addTextChangedListener(changeUsernameTextWatcher);
        changeUsername.setOnFocusChangeListener((v, hasFocus) -> validateUsername(changeUsernameInput, changeUsername));
    }

    public void initialiseChangePassword(){
        changePassword.addTextChangedListener(changePasswordTextWatcher);
        changePassword.setOnFocusChangeListener((v, hasFocus) -> validatePassword(changePasswordInput, changePassword, changePasswordConfirm, false));

        changePasswordConfirm.addTextChangedListener(changePasswordTextWatcher);
        changePasswordConfirm.setOnFocusChangeListener((v, hasFocus) -> validatePassword(changePasswordConfirmInput, changePasswordConfirm, changePassword, false));
    }

    public void initialiseDeleteUser(){
        deletePassword.addTextChangedListener(deleteUserTextWatcher);
        deletePassword.setOnFocusChangeListener((v, hasFocus) -> validatePassword(deletePasswordInput, deletePassword, deletePasswordConfirm, true));

        deletePasswordConfirm.addTextChangedListener(deleteUserTextWatcher);
        deletePasswordConfirm.setOnFocusChangeListener((v, hasFocus) -> validatePassword(deletePasswordConfirmInput, deletePasswordConfirm, deletePassword, true));
    }

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

    public boolean validateUsername(TextInputLayout textInputLayout, EditText editText){
        String usernameText = editText.getText().toString();
        boolean hasFocus = editText.hasFocus();
        boolean emptyUsername = usernameText.isEmpty();
        boolean validUsername = !emptyUsername && accountViewModal.validateUsername(usernameText);

        if(!hasFocus || validUsername)
            textInputLayout.setErrorEnabled(false);
        else if(emptyUsername)
            textInputLayout.setError("Username cannot be empty");
        else
            textInputLayout.setError("Username already taken");
        return validUsername;
    }

    public boolean validatePassword(TextInputLayout textInputLayout, EditText editText1, EditText editText2, Boolean equal){
        String oldPassword = accountViewModal.getUser().getPassword();
        String newPasswordText = editText1.getText().toString();
        String newPasswordConfirmText = editText2.getText().toString();

        boolean hasFocus = editText1.hasFocus();
        boolean emptyPassword = newPasswordText.isEmpty();
        boolean validPassword = !emptyPassword && newPasswordText.equals(newPasswordConfirmText);
        boolean equalPassword = equal == null || equal == newPasswordText.equals(oldPassword);

        if(!hasFocus || (validPassword && equalPassword))
            textInputLayout.setErrorEnabled(false);
        else if(emptyPassword)
            textInputLayout.setError("Password cannot be empty");
        else if(!validPassword)
            textInputLayout.setError("Both passwords must match");
        else
            textInputLayout.setError("Password must " + (equal ? "" : "not ") + "match old password");
        return validPassword && equalPassword;
    }

    private final TextWatcher newUserTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            boolean validUsername = validateUsername(newUsernameInput, newUsername);
            boolean validPassword = validatePassword(newPasswordInput, newPassword, newPasswordConfirm, null);
            boolean validPasswordConfirm = validatePassword(newPasswordConfirmInput, newPasswordConfirm, newPassword, null);
            newUserButton.setEnabled(validUsername && validPassword && validPasswordConfirm);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    private final TextWatcher changeUsernameTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            boolean validUsername = validateUsername(changeUsernameInput, changeUsername);
            changeUsernameButton.setEnabled(validUsername);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    private final TextWatcher changePasswordTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            boolean validPassword = validatePassword(changePasswordInput, changePassword, changePasswordConfirm, false);
            boolean validPasswordConfirm = validatePassword(changePasswordConfirmInput, changePasswordConfirm, changePassword, false);
            changePasswordButton.setEnabled(validPassword && validPasswordConfirm);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    private final TextWatcher deleteUserTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            boolean validPassword = validatePassword(deletePasswordInput, deletePassword, deletePasswordConfirm, true);
            boolean validPasswordConfirm = validatePassword(deletePasswordConfirmInput, deletePasswordConfirm, deletePassword, true);
            deleteUserButton.setEnabled(validPassword && validPasswordConfirm);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };
}
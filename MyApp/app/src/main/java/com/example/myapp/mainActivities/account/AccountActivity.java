package com.example.myapp.mainActivities.account;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.graphics.Paint;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Pair;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.MainApplication;
import com.example.myapp.R;
import com.example.myapp.databasefiles.user.User;
import com.example.myapp.mainActivities.MusicActivity;
import com.google.android.material.textfield.TextInputLayout;

import java.io.File;
import java.time.LocalDateTime;
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
        accountViewModel = new ViewModelProvider(this).get(AccountViewModel.class);
        accountViewModel.loadUser(getIntent().getExtras().getInt("userID"));
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseViewByID();
        initialiseLinearLayouts();
        initialiseTextInputs();
        initialiseButtons();
        reloadPage();
    }

    public void initialiseViewByID(){
        titleText = findViewById(R.id.titleText);
        loginButton = findViewById(R.id.loginButton);
        initialiseCreationView();
        initialiseUsernameView();
        initialisePasswordView();
        initialiseDeletionView();
    }

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

    public void initialiseUsernameView(){
        layoutUsernameVisible = findViewById(R.id.layoutUsernameVisible);
        layoutUsernameHidden = findViewById(R.id.layoutUsernameHidden);
        changeUsernameTitle = findViewById(R.id.changeUsernameTitle);
        changeUsernameInput = findViewById(R.id.changeUsernameInput);
        changeUsername = findViewById(R.id.changeUsername);
        changeUsernameButton = findViewById(R.id.changeUsernameButton);
    }

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

    public void initialiseLinearLayouts(){
        setupLayouts(layoutCreationVisible, layoutCreationHidden);
        setupLayouts(layoutUsernameVisible, layoutUsernameHidden);
        setupLayouts(layoutPasswordVisible, layoutPasswordHidden);
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

    public void initialiseTextInputs(){
        initialiseNewUser();
        initialiseChangeUsername();
        initialiseChangePassword();
        initialiseDeleteUser();
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

    public void initialiseButtons(){
        initialiseCreateButton();
        initialiseChangeUsernameButton();
        initialiseChangePasswordButton();
        initialiseDeleteButton();
        initialiseLoginButton();
    }

    public void initialiseCreateButton(){
        newUserButton.setOnClickListener(v -> {
            String usernameText = newUsername.getText().toString();
            String passwordText = newPassword.getText().toString();
            accountViewModel.insert(new User(usernameText, passwordText));
            createMusicFolder();
            createLogsFile();
            reloadPage();
            accountViewModel.updateSaveLogs(new Pair<>(usernameText + "account created", LocalDateTime.now()));
        });
    }

    public void initialiseChangeUsernameButton(){
        changeUsernameButton.setOnClickListener(v -> {
            String usernameText = changeUsername.getText().toString();
            accountViewModel.changeUsername(usernameText);
            Toast.makeText(getApplicationContext(), "Username changed", Toast.LENGTH_SHORT).show();
            reloadPage();
            accountViewModel.updateSaveLogs(new Pair<>("Username changed", LocalDateTime.now()));
        });
    }

    public void initialiseChangePasswordButton(){
        changePasswordButton.setOnClickListener(v -> {
            String passwordText = changePassword.getText().toString();
            accountViewModel.changePassword(passwordText);
            Toast.makeText(getApplicationContext(), "Password changed", Toast.LENGTH_SHORT).show();
            reloadPage();
        });
    }

    public void initialiseDeleteButton(){
        deleteUserButton.setOnClickListener(view -> new AlertDialog.Builder(this)
                .setTitle("Account Deletion")
                .setMessage("Are you sure you want to delete your account? There is no way to recover your account once deleted.")
                .setPositiveButton("Yes", (dialog, which) -> {
                    int userID = accountViewModel.delete();
                    deleteFolder(userID);
                    reloadPage();
                    accountViewModel.updateSaveLogs(new Pair<>("Account deleted", LocalDateTime.now()));
                })
                .setNegativeButton("No", null)
                .create()
                .show());
    }

    public void initialiseLoginButton(){
        loginButton.setOnClickListener(v -> {
            ((MainApplication) this.getApplication()).setUserID(accountViewModel.getUser().getUserID());
            startActivity(new Intent(getApplicationContext(), MusicActivity.class));
            Toast.makeText(getApplicationContext(), "Welcome " + accountViewModel.getUser().getUsername(), Toast.LENGTH_SHORT).show();
        });
    }

    @SuppressLint("SetTextI18n")
    public void reloadPage(){
        User user = accountViewModel.getUser();
        int userID = user.getUserID();
        titleText.setText("Welcome " + user.getUsername());
        loginButton.setEnabled(userID >= 0);
        clearTextFields();
        clearFocus();
        hideLayouts(userID);
    }

    public void hideLayouts(int userID){
        hideLayout(layoutCreationVisible, layoutCreationHidden, accountCreationTitle, userID < 0);
        hideLayout(layoutUsernameVisible, layoutUsernameHidden, changeUsernameTitle, userID > 0);
        hideLayout(layoutPasswordVisible, layoutPasswordHidden, changePasswordTitle, userID > 0);
        hideLayout(layoutDeletionVisible, layoutDeletionHidden, accountDeletionTitle, userID > 0);
    }

    public void hideLayout(LinearLayout layoutVisible, LinearLayout layoutHidden, TextView title, boolean clickable){
        layoutHidden.setVisibility(View.GONE);
        layoutVisible.setEnabled(clickable);
        int paintFlags = clickable ? title.getPaintFlags() & (~Paint.STRIKE_THRU_TEXT_FLAG) : title.getPaintFlags() | Paint.STRIKE_THRU_TEXT_FLAG;
        title.setPaintFlags(paintFlags);
    }

    public void createMusicFolder(){
        int userID = accountViewModel.getUser().getUserID();
        String musicFolderPath = accountViewModel.getMusicFilePath() + userID;
        File musicFolder = new File(musicFolderPath);
        boolean musicFolderCreation = musicFolder.mkdirs();
        Toast.makeText(getApplicationContext(), "Folder creation " + (musicFolderCreation ? "successful" : "failed"), Toast.LENGTH_SHORT).show();
    }

    public void createLogsFile(){
        int userID = accountViewModel.getUser().getUserID();
        try{
            File logFile = new File(accountViewModel.getLogsFilePath(), userID + ".txt");
            boolean logFileCreation = logFile.createNewFile();
            Toast.makeText(getApplicationContext(), "File creation " + (logFileCreation ? "successful" : "failed"), Toast.LENGTH_SHORT).show();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void deleteFolder(int userID){
        File musicFolder = new File(accountViewModel.getMusicFilePath() + userID);
        if(musicFolder.isDirectory()){
            String[] children = musicFolder.list();
            for (String child : children) {
                System.out.println(child);
                new File(musicFolder, child).delete();
            }
        }
        boolean folderDeletion = musicFolder.delete();
        Toast.makeText(getApplicationContext(), "Folder deletion " + (folderDeletion ? "successful" : "failed"), Toast.LENGTH_SHORT).show();

        File logFile = new File(new File(accountViewModel.getLogsFilePath()), userID + ".txt");
        boolean fileDeletion = logFile.delete();
        Toast.makeText(getApplicationContext(), "File deletion " + (fileDeletion ? "successful" : "failed"), Toast.LENGTH_SHORT).show();
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

    public boolean validateUsername(TextInputLayout textInputLayout, EditText editText){
        String usernameText = editText.getText().toString();
        boolean hasFocus = editText.hasFocus();
        boolean emptyUsername = usernameText.isEmpty();
        boolean validUsername = !emptyUsername && accountViewModel.validateUsername(usernameText);

        if(!hasFocus || validUsername)
            textInputLayout.setErrorEnabled(false);
        else if(emptyUsername)
            textInputLayout.setError("Username cannot be empty");
        else
            textInputLayout.setError("Username already taken");
        return validUsername;
    }

    public boolean validatePassword(TextInputLayout textInputLayout, EditText editText1, EditText editText2, Boolean equal){
        String oldPassword = accountViewModel.getUser().getPassword();
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
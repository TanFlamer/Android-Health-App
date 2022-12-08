package com.example.myapp.mainActivities.login;

import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.widget.Button;
import android.widget.EditText;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.google.android.material.textfield.TextInputLayout;

public class LoginActivity extends AppCompatActivity {

    LoginViewModel loginViewModel;
    EditText username, password;
    Button buttonLogin, buttonNew, buttonGuest;
    TextInputLayout usernameInput, passwordInput;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_login);
        //get view model
        loginViewModel = new ViewModelProvider(this).get(LoginViewModel.class);
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //link all components with ID
        initialiseViewByID();
        //initialise edit text for user input
        initialiseTextInputs();
        //initialise all buttons
        initialiseButtons();
    }

    //link all components with ID
    public void initialiseViewByID(){
        username = findViewById(R.id.username);
        usernameInput = findViewById(R.id.playlistNameInput);
        password = findViewById(R.id.password);
        passwordInput = findViewById(R.id.passwordInput);
        buttonLogin = findViewById(R.id.buttonLogin);
        buttonNew = findViewById(R.id.buttonNew);
        buttonGuest = findViewById(R.id.buttonGuest);
    }

    //initialise edit text for user input
    public void initialiseTextInputs(){
        //initialise username edit text
        initialiseUsername();
        //initialise password edit text
        initialisePassword();
    }

    //initialise username edit text
    public void initialiseUsername(){
        //add text watcher
        username.addTextChangedListener(loginTextWatcher);
        //add focus listener
        username.setOnFocusChangeListener((v, hasFocus) -> validateInput(usernameInput, username));
    }

    //initialise password edit text
    public void initialisePassword(){
        //add text watcher
        password.addTextChangedListener(loginTextWatcher);
        //add focus listener
        password.setOnFocusChangeListener((v, hasFocus) -> validateInput(passwordInput, password));
    }

    //initialise all buttons
    public void initialiseButtons(){
        //initialise login button
        initialiseLoginButton();
        //initialise new button
        initialiseNewButton();
        //initialise guest button
        initialiseGuestButton();
    }

    //initialise login button
    public void initialiseLoginButton(){
        //login button on click listener
        buttonLogin.setOnClickListener(view -> {
            //get username text from edit text
            String usernameText = username.getText().toString();
            //get password text from edit text
            String passwordText = password.getText().toString();
            //validate if user exists
            Intent intent = loginViewModel.validateUser(usernameText, passwordText);
            //if user exists, move to next activity
            if(intent != null) startActivity(intent);
            //clear edit text and remove focus
            clearTextFields();
        });
    }

    //initialise new button
    public void initialiseNewButton(){
        //new button on click listener
        buttonNew.setOnClickListener(v -> {
            //move to next activity to create new account
            startActivity(loginViewModel.loginNewUser());
            //clear edit text and remove focus
            clearTextFields();
        });
    }

    //initialise guest button
    public void initialiseGuestButton(){
        //guest button on click listener
        buttonGuest.setOnClickListener(view -> {
            //display dialog to validate login as guest
            loginViewModel.guestDialog(view.getContext()).show();
            //clear edit text and remove focus
            clearTextFields();
        });
    }

    //clear edit text and remove focus
    public void clearTextFields(){
        //clear username edit text
        username.getText().clear();
        //clear username focus
        username.clearFocus();
        //clear password edit text
        password.getText().clear();
        //clear password focus
        password.clearFocus();
    }

    //validate text input
    public boolean validateInput(TextInputLayout textInputLayout, EditText editText){
        //get text input from edit text
        String text = editText.getText().toString();
        //check if edit text has focus
        boolean hasFocus = editText.hasFocus();
        //check if edit text is empty
        boolean emptyText = text.isEmpty();

        if(!(hasFocus && emptyText)) //if edit text has no focus or is not empty
            textInputLayout.setErrorEnabled(false); //show no error
        else //else if edit text has focus and is empty
            textInputLayout.setError("Field cannot be empty"); //show error
        return emptyText; //return edit text empty state
    }

    //text watcher for username and password
    private final TextWatcher loginTextWatcher = new TextWatcher() {

        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            //check if username is empty
            boolean emptyUsername = validateInput(usernameInput, username);
            //check if password is empty
            boolean emptyPassword = validateInput(passwordInput, password);
            //enable login button if username and password are not empty
            buttonLogin.setEnabled(!(emptyUsername || emptyPassword));
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };
}
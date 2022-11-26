package com.example.myapp.mainActivities.login;

import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Pair;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.user.User;
import com.example.myapp.mainActivities.account.Account;
import com.google.android.material.textfield.TextInputLayout;

import java.time.LocalTime;

public class Login extends AppCompatActivity {

    LoginViewModel loginViewModel;
    EditText username, password;
    Button buttonLogin, buttonNew, buttonGuest;
    TextInputLayout usernameInput, passwordInput;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_login);

        loginViewModel = new ViewModelProvider(this).get(LoginViewModel.class);
        initialiseAll();
        loginViewModel.updateSaveLogs(new Pair<>("Entered Login screen.", LocalTime.now()));
    }

    public void initialiseAll(){
        initialiseUsername();
        initialisePassword();
        initialiseButtons();
    }

    public void initialiseUsername(){
        username = findViewById(R.id.username);
        usernameInput = findViewById(R.id.playlistNameInput);
        username.addTextChangedListener(loginTextWatcher);
        username.setOnFocusChangeListener((v, hasFocus) -> validateInput(usernameInput, username));
    }

    public void initialisePassword(){
        password = findViewById(R.id.password);
        passwordInput = findViewById(R.id.passwordInput);
        password.addTextChangedListener(loginTextWatcher);
        password.setOnFocusChangeListener((v, hasFocus) -> validateInput(passwordInput, password));
    }

    public void initialiseButtons(){
        initialiseLoginButton();
        initialiseNewButton();
        initialiseGuestButton();
    }

    public void initialiseLoginButton(){
        buttonLogin = findViewById(R.id.buttonLogin);
        buttonLogin.setOnClickListener(view -> {
            User user = loginViewModel.validateUser(username.getText().toString(), password.getText().toString());
            if(user == null)
                Toast.makeText(this, "Invalid Login Credentials", Toast.LENGTH_SHORT).show();
            else {
                Intent intent = new Intent(getApplicationContext(), Account.class);
                sendUserData(intent, user);
                Toast.makeText(this, "Login Successful", Toast.LENGTH_SHORT).show();
                startActivity(intent);
            }
            clearTextFields();
        });
    }

    public void initialiseNewButton(){
        buttonNew = findViewById(R.id.buttonNew);
        buttonNew.setOnClickListener(v -> {
            Intent intent = new Intent(getApplicationContext(), Account.class);
            User user = new User(-1, "User", null);
            sendUserData(intent, user);
            startActivity(intent);
            clearTextFields();
        });
    }

    public void initialiseGuestButton(){
        buttonGuest = findViewById(R.id.buttonGuest);
        buttonGuest.setOnClickListener(view -> new AlertDialog.Builder(this)
                .setTitle("Guest Login")
                .setMessage("Are you sure you want to login as guest? Any changes made will not be saved.")
                .setPositiveButton("Yes", (dialogInterface, i) -> {
                    Intent intent = new Intent(getApplicationContext(), Account.class);
                    User user = new User(0, "Guest", null);
                    sendUserData(intent, user);
                    startActivity(intent);
                    clearTextFields();
                })
                .setNegativeButton("No", null)
                .create()
                .show());
    }

    public void sendUserData(Intent intent, User user){
        intent.putExtra("userID", user.getUserID());
        intent.putExtra("username", user.getUsername());
        intent.putExtra("password", user.getPassword());
    }

    public void clearTextFields(){
        username.getText().clear();
        username.clearFocus();
        password.getText().clear();
        password.clearFocus();
    }

    public boolean validateInput(TextInputLayout textInputLayout, EditText editText){
        String text = editText.getText().toString();
        boolean hasFocus = editText.hasFocus();
        boolean emptyText = text.isEmpty();

        if(!(hasFocus && emptyText))
            textInputLayout.setErrorEnabled(false);
        else
            textInputLayout.setError("Field cannot be empty");
        return emptyText;
    }

    private final TextWatcher loginTextWatcher = new TextWatcher() {

        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            boolean emptyUsername = validateInput(usernameInput, username);
            boolean emptyPassword = validateInput(passwordInput, password);
            buttonLogin.setEnabled(!(emptyUsername || emptyPassword));
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };
}
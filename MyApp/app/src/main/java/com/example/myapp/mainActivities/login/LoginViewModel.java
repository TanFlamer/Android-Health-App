package com.example.myapp.mainActivities.login;

import android.app.Application;
import android.content.Intent;
import android.util.Pair;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.user.User;
import com.example.myapp.databasefiles.user.UserRepository;
import com.example.myapp.mainActivities.account.AccountActivity;

import java.time.LocalDateTime;
import java.time.LocalTime;

public class LoginViewModel extends AndroidViewModel {

    private final UserRepository userRepository;

    public LoginViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = (MainApplication) getApplication();
        userRepository = mainApplication.getUserRepository();
    }

    public Intent validateUser(String username, String password){
        User user = userRepository.findUser(username);
        if(user != null && user.getPassword().equals(password))
            return loginUser(user);
        else{
            Toast.makeText(getApplication(), "Invalid Login Credentials", Toast.LENGTH_SHORT).show();
            return null;
        }
    }

    public Intent loginGuest(){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        intent.putExtra("userID", 0);
        return intent;
    }

    public Intent loginNewUser(){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        intent.putExtra("userID", -1);
        return intent;
    }

    public Intent loginUser(User user){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        intent.putExtra("userID", user.getUserID());
        return intent;
    }
}

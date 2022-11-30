package com.example.myapp.mainActivities.login;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.user.User;
import com.example.myapp.databasefiles.user.UserRepository;

import java.time.LocalDateTime;
import java.time.LocalTime;

public class LoginViewModel extends AndroidViewModel {

    private final UserRepository userRepository;

    public LoginViewModel(@NonNull Application application) {
        super(application);
        userRepository = ((MainApplication) getApplication()).getUserRepository();
    }

    public User validateUser(String username, String password){
        User user = userRepository.findUser(username);
        return (user != null && user.getPassword().equals(password)) ? user : null;
    }

    public void updateSaveLogs(Pair<String, LocalDateTime> newSaveLog){
        ((MainApplication) getApplication()).updateSaveLogs(newSaveLog);
    }
}

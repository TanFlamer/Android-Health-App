package com.example.test;

import android.app.Application;

import androidx.lifecycle.LiveData;

import java.util.List;

public class MainApplication extends Application {

    private UserRepository userRepository;
    private LiveData<List<User>> allUsers;

    @Override
    public void onCreate() {
        super.onCreate();
        userRepository = new UserRepository(this);
        allUsers = userRepository.getAllUsers();
    }

    public UserRepository getUserRepository() {
        return userRepository;
    }
}

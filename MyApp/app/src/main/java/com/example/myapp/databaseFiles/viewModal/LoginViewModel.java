package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.User;
import com.example.myapp.databaseFiles.repository.UserRepository;

import java.time.LocalTime;
import java.util.List;

public class LoginViewModel extends AndroidViewModel {

    private final UserRepository userRepository;

    public LoginViewModel(@NonNull Application application) {
        super(application);
        userRepository = new UserRepository(application);
    }

    public User validateUser(String username, String password){
        List<User> user = userRepository.findUser(username);
        return (user.size() != 0 && user.get(0).getPassword().equals(password)) ? user.get(0) : null;
    }

    public void updateSaveLogs(Pair<String, LocalTime> newSaveLog){
        ((MainApplication) getApplication()).updateSaveLogs(newSaveLog);
    }
}

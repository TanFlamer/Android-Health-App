package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.databaseFiles.entity.User;
import com.example.myapp.databaseFiles.repository.UserRepository;

import java.util.List;

public class LoginViewModel extends AndroidViewModel {

    private UserRepository userRepository;

    public LoginViewModel(@NonNull Application application) {
        super(application);
        userRepository = new UserRepository(application);
    }

    public User validateUser(String username, String password){
        List<User> user = userRepository.findUser(username);
        return (user.size() != 0 && user.get(0).getPassword().equals(password)) ? user.get(0) : null;
    }
}

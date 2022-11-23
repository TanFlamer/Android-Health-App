package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;
import androidx.lifecycle.Observer;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.User;
import com.example.myapp.databaseFiles.repository.UserRepository;

import java.time.LocalTime;

public class AccountViewModel extends AndroidViewModel {

    private final UserRepository userRepository;
    private final String filePath;
    private User user;

    public AccountViewModel(@NonNull Application application) {
        super(application);
        userRepository = ((MainApplication) getApplication()).getUserRepository();
        filePath = getApplication().getFilesDir() + "/music/";
        user = new User(-1, "User", null);
    }

    public void loadUser(Integer userID, String username, String password){
        user = new User(userID, username, password);
    }

    public void insert(User newUser) {
        long userID = userRepository.insert(newUser);
        user = new User((int) userID, newUser.getUsername(), newUser.getPassword());
    }

    public int delete(){
        int userID = user.getUserID();
        userRepository.delete(user);
        user = new User(-1, "User", null);
        return userID;
    }

    public void changeUsername(String newUsername){
        user.setUsername(newUsername);
        userRepository.update(user);
    }

    public void changePassword(String newPassword){
        user.setPassword(newPassword);
        userRepository.update(user);
    }

    public boolean validateUsername(String username){
        return userRepository.findUser(username).size() == 0;
    }

    public User getUser() {
        return user;
    }

    public String getFilePath() {
        return filePath;
    }

    public void updateSaveLogs(Pair<String, LocalTime> newSaveLog){
        ((MainApplication) getApplication()).updateSaveLogs(newSaveLog);
    }
}

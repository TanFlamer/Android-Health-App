package com.example.myapp.mainActivities.account;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.user.User;
import com.example.myapp.databasefiles.user.UserRepository;

import java.time.LocalDateTime;
import java.time.LocalTime;

public class AccountViewModel extends AndroidViewModel {

    private final UserRepository userRepository;
    private final String filePath;
    private User user;

    public AccountViewModel(@NonNull Application application) {
        super(application);
        userRepository = ((MainApplication) getApplication()).getUserRepository();
        filePath = getApplication().getFilesDir().toString();
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
        return userRepository.findUser(username) == null;
    }

    public User getUser() {
        return user;
    }

    public String getMusicFilePath() {
        return filePath + "/music/";
    }

    public String getLogsFilePath() {
        return filePath + "/logs";
    }

    public void updateSaveLogs(Pair<String, LocalDateTime> newSaveLog){
        ((MainApplication) getApplication()).updateSaveLogs(newSaveLog);
    }
}

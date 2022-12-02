package com.example.myapp.mainActivities.account;

import android.app.Application;
import android.content.Intent;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.user.User;
import com.example.myapp.databasefiles.user.UserRepository;
import com.example.myapp.mainActivities.MusicActivity;

import java.io.File;

public class AccountViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final UserRepository userRepository;
    private final String filePath;
    private final User newUser;
    private User user;

    public AccountViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        userRepository = mainApplication.getUserRepository();
        filePath = getApplication().getFilesDir().toString();
        newUser = userRepository.getUser(-1);
    }

    public void loadUser(Integer userID){
        user = userRepository.getUser(userID);
    }

    public boolean validateUsername(String username){
        return userRepository.findUser(username) == null;
    }

    public void createUser(String username, String password){
        long userID = userRepository.insert(new User(username, password));
        user = new User((int) userID, username, password);
        createMusicFolder();
        mainApplication.updateSaveLogs(username + " account created");
    }

    public void changeUsername(String newUsername){
        user.setUsername(newUsername);
        userRepository.update(user);
        mainApplication.updateSaveLogs("Username changed to " + newUsername);
    }

    public void changePassword(String newPassword){
        user.setPassword(newPassword);
        userRepository.update(user);
        mainApplication.updateSaveLogs("Password changed");
    }

    public void deleteUser(){
        deleteMusicFolder();
        deleteLogFiles();
        userRepository.delete(user);
        user = newUser;
    }

    public Intent loginUser(){
        mainApplication.setUserID(user.getUserID());
        Toast.makeText(getApplication(), "Welcome " + user.getUsername(), Toast.LENGTH_SHORT).show();
        return new Intent(getApplication(), MusicActivity.class);
    }

    public void createMusicFolder(){
        File musicFolder = new File(getMusicPath());
        musicFolder.mkdirs();
    }

    public void deleteMusicFolder(){
        File musicFolder = new File(getMusicPath());
        if(musicFolder.isDirectory()){
            String[] children = musicFolder.list();
            for (String child : children) {
                File musicFile = new File(musicFolder, child);
                musicFile.delete();
            }
        }
        musicFolder.delete();
    }

    public void deleteLogFiles(){
        File logFile = new File(getLogsPath());
        logFile.delete();
    }

    public String getMusicPath(){
        return filePath + "/music/" + user.getUserID();
    }

    public String getLogsPath(){
        return filePath + "/logs/" + user.getUserID() + ".txt";
    }

    public User getUser() {
        return user;
    }

    public void resetMusicPlayer(){
        mainApplication.getMusicPlayer().resetMediaPlayer();
    }
}

package com.example.myapp.mainActivities.account;

import android.app.Application;
import android.content.Intent;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.user.User;
import com.example.myapp.databaseFiles.user.UserRepository;
import com.example.myapp.mainActivities.MusicActivity;

import java.io.File;

public class AccountViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final UserRepository userRepository;
    private final String filePath;
    private User user;

    //constructor for view model
    public AccountViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        userRepository = mainApplication.getUserRepository();
        filePath = getApplication().getFilesDir().toString();
    }

    //load user from database
    public void loadUser(String username){
        user = userRepository.findUser(username);
        mainApplication.setUserID(user.getUserID());
    }

    //check if username taken
    public boolean validateUsername(String username){
        return userRepository.findUser(username) == null;
    }

    //create new user
    public void createUser(String username, String password){
        long userID = userRepository.insert(new User(username, password));
        user = new User((int) userID, username, password);
        //create new music folder for new user
        createMusicFolder();
        mainApplication.setUserID(user.getUserID());
        //update logs for new user
        mainApplication.updateSaveLogs(username + " account created");
    }

    //change username
    public void changeUsername(String newUsername){
        user.setUsername(newUsername);
        userRepository.update(user);
        //update logs for user
        mainApplication.addLogs("Username changed to " + newUsername);
    }

    //change password
    public void changePassword(String newPassword){
        user.setPassword(newPassword);
        userRepository.update(user);
        //update logs for user
        mainApplication.addLogs("Password changed");
    }

    //delete user
    public void deleteUser(){
        //delete user music files
        deleteMusicFolder();
        //delete user log files
        deleteLogFiles();
        userRepository.delete(user);
        //reset user to new user
        user = userRepository.findUser("NEW USER");
        mainApplication.setUserID(0);
        //reset logs
        mainApplication.resetLogs();
    }

    //login user to app
    public Intent loginUser(){
        mainApplication.setUserID(user.getUserID());
        mainApplication.addLogs("Login into app");
        Toast.makeText(getApplication(), "Welcome " + user.getUsername(), Toast.LENGTH_SHORT).show();
        return new Intent(getApplication(), MusicActivity.class);
    }

    //create new music folder for new user
    public void createMusicFolder(){
        File musicFolder = new File(getMusicPath());
        musicFolder.mkdirs();
    }

    //delete music folder of deleted user
    public void deleteMusicFolder(){
        //get file path to music folder
        File musicFolder = new File(getMusicPath());
        if(musicFolder.isDirectory()){
            String[] children = musicFolder.list();
            //delete all music files inside folder
            for (String child : children) {
                File musicFile = new File(musicFolder, child);
                musicFile.delete();
            }
        }
        //delete empty music folder
        musicFolder.delete();
    }

    //delete logs file of deleted user
    public void deleteLogFiles(){
        File logFile = new File(getLogsPath());
        logFile.delete();
    }

    //get file path to music folder
    public String getMusicPath(){
        return filePath + "/music/" + user.getUserID();
    }

    //get file path to logs file
    public String getLogsPath(){
        return filePath + "/logs/" + user.getUserID() + ".txt";
    }

    //return user
    public User getUser() {
        return user;
    }

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }

    //reset music player and logs when page resumes
    public void resetApp(){
        mainApplication.getMusicPlayer().resetMediaPlayer();
        mainApplication.resetLogs();
    }
}

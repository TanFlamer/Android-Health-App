package com.example.myapp.databaseFiles.user;

import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

@Entity(tableName = "Users")
public class User {

    @PrimaryKey(autoGenerate = true)
    private Integer userID; //unique ID for each user

    private String username; //unique username for each user

    private String password; //password for each user

    //constructor for new user
    public User(String username, String password) {
        this.username = username;
        this.password = password;
    }

    @Ignore //constructor to change username or password
    public User(Integer userID, String username, String password) {
        this.userID = userID;
        this.username = username;
        this.password = password;
    }

    //getter for user ID
    public Integer getUserID() {
        return userID;
    }

    //setter for user ID
    public void setUserID(Integer userID) {
        this.userID = userID;
    }

    //getter for username
    public String getUsername() {
        return username;
    }

    //setter for username
    public void setUsername(String username) {
        this.username = username;
    }

    //getter for user password
    public String getPassword() {
        return password;
    }

    //setter for user password
    public void setPassword(String password) {
        this.password = password;
    }
}

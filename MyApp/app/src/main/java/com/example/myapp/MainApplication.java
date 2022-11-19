package com.example.myapp;

import android.app.Application;

public class MainApplication extends Application {

    int userID;

    public int getUserID() {
        return userID;
    }

    public void setUserID(int userID) {
        this.userID = userID;
    }
}

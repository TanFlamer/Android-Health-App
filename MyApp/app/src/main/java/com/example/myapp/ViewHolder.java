package com.example.myapp;

import android.view.View;

import java.util.HashMap;

public class ViewHolder {

    //hashmap to store initialised views
    private final HashMap<Integer, View> storedViews = new HashMap<>();

    //add new initialised view to hashmap
    public void addView(View view) {
        int id = view.getId();
        storedViews.put(id, view);
    }

    //get initialised view from hashmap
    public View getView(int id) {
        return storedViews.get(id);
    }
}

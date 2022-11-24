package com.example.myapp.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Ignore;
import androidx.room.Index;
import androidx.room.PrimaryKey;

@Entity(tableName = "Types",
        indices = @Index("userID"),
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Type {

    @PrimaryKey(autoGenerate = true)
    private Integer typeID;

    private String name;

    private Double caloriePerMinute;

    private Integer userID;

    public Type(String name, Double caloriePerMinute, Integer userID) {
        this.name = name;
        this.caloriePerMinute = caloriePerMinute;
        this.userID = userID;
    }

    @Ignore
    public Type(Integer typeID, String name, Double caloriePerMinute, Integer userID) {
        this.typeID = typeID;
        this.name = name;
        this.caloriePerMinute = caloriePerMinute;
        this.userID = userID;
    }

    public Integer getTypeID() {
        return typeID;
    }

    public void setTypeID(Integer typeID) {
        this.typeID = typeID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Double getCaloriePerMinute() {
        return caloriePerMinute;
    }

    public void setCaloriePerMinute(Double caloriePerMinute) {
        this.caloriePerMinute = caloriePerMinute;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}

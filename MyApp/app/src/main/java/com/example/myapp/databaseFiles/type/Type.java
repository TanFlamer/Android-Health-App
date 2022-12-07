package com.example.myapp.databasefiles.type;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Ignore;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.example.myapp.databasefiles.user.User;

@Entity(tableName = "Types",
        indices = { @Index("userID"), @Index("typeName") },
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Type {

    @PrimaryKey(autoGenerate = true)
    private Integer typeID; //unique ID for each sport type regardless of user

    private String typeName; //unique name for each sport type (same name allowed for different user)

    private Double caloriePerMinute; //calories burned per minute for each sport type

    private Integer userID; //user ID of the user who the sport type belongs to

    //constructor for new sport type
    public Type(String typeName, Double caloriePerMinute, Integer userID) {
        this.typeName = typeName;
        this.caloriePerMinute = caloriePerMinute;
        this.userID = userID;
    }

    @Ignore //constructor to change sport type
    public Type(Integer typeID, String typeName, Double caloriePerMinute, Integer userID) {
        this.typeID = typeID;
        this.typeName = typeName;
        this.caloriePerMinute = caloriePerMinute;
        this.userID = userID;
    }

    //getter for sport type ID
    public Integer getTypeID() {
        return typeID;
    }

    public void setTypeID(Integer typeID) {
        this.typeID = typeID;
    }

    //getter for sport type name
    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    //getter for sport calorie per minute
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

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
    private Integer typeID;

    private String typeName;

    private Double caloriePerMinute;

    private Integer userID;

    public Type(String typeName, Double caloriePerMinute, Integer userID) {
        this.typeName = typeName;
        this.caloriePerMinute = caloriePerMinute;
        this.userID = userID;
    }

    @Ignore
    public Type(Integer typeID, String typeName, Double caloriePerMinute, Integer userID) {
        this.typeID = typeID;
        this.typeName = typeName;
        this.caloriePerMinute = caloriePerMinute;
        this.userID = userID;
    }

    public Integer getTypeID() {
        return typeID;
    }

    public void setTypeID(Integer typeID) {
        this.typeID = typeID;
    }

    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
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

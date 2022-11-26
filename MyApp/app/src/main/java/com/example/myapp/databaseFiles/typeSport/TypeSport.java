package com.example.myapp.databaseFiles.typeSport;

import static androidx.room.ForeignKey.CASCADE;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;

import com.example.myapp.databaseFiles.user.User;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;

@Entity(tableName = "TypeSport",
        primaryKeys = { "sportID", "typeID" },
        indices = { @Index("sportID"), @Index("typeID"), @Index("userID") },
        foreignKeys = { @ForeignKey(entity = Sport.class, parentColumns = "sportID", childColumns = "sportID", onDelete = CASCADE),
                        @ForeignKey(entity = Type.class, parentColumns = "typeID", childColumns = "typeID", onDelete = CASCADE),
                        @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE) })
public class TypeSport {

    @NonNull
    private Integer sportID;

    @NonNull
    private Integer typeID;

    private Integer sportDuration;

    @NonNull
    private Integer userID;

    public TypeSport(@NonNull Integer sportID, @NonNull Integer typeID, Integer sportDuration, @NonNull Integer userID){
        this.sportID = sportID;
        this.typeID = typeID;
        this.sportDuration = sportDuration;
        this.userID = userID;
    }

    @NonNull
    public Integer getSportID() {
        return sportID;
    }

    public void setSportID(@NonNull Integer sportID) {
        this.sportID = sportID;
    }

    @NonNull
    public Integer getTypeID() {
        return typeID;
    }

    public void setTypeID(@NonNull Integer typeID) {
        this.typeID = typeID;
    }

    public Integer getSportDuration() {
        return sportDuration;
    }

    public void setSportDuration(Integer sportDuration) {
        this.sportDuration = sportDuration;
    }

    @NonNull
    public Integer getUserID() {
        return userID;
    }

    public void setUserID(@NonNull Integer userID) {
        this.userID = userID;
    }
}

package com.example.myapp.databaseFiles.song;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import java.util.List;

@Dao
public interface SongDao {

    @Insert
    void insert(Song song); //insert operation for new song

    @Update
    void update(Song song); //update operation for existing song

    @Delete
    void delete(Song song); //delete operation for existing song

    @Query("SELECT * FROM Songs WHERE userID=:userID AND songName=:songName")
    Song findSong(int userID, String songName); //check if song with specific name exists for a user

    @Query("SELECT * FROM Songs WHERE userID=:userID")
    LiveData<List<Song>> getAllSongs(int userID); //returns live data of all songs belonging to a user
}

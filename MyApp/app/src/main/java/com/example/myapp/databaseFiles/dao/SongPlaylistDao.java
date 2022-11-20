package com.example.myapp.databaseFiles.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.myapp.databaseFiles.entity.SongPlaylist;

import java.util.List;

@Dao
public interface SongPlaylistDao {

    @Insert
    void insert(SongPlaylist songPlaylist);

    @Update
    void update(SongPlaylist songPlaylist);

    @Delete
    void delete(SongPlaylist songPlaylist);

    @Query("SELECT * FROM SongPlaylist WHERE playlistID=:playlistID AND songID=:songID")
    List<SongPlaylist> findSongPlaylist(int playlistID, int songID);

    @Query("SELECT * FROM SongPlaylist WHERE playlistID=:playlistID")
    LiveData<List<SongPlaylist>> getSongPlaylist(int playlistID);

    @Query("SELECT * FROM SongPlaylist WHERE userID=:userID")
    LiveData<List<SongPlaylist>> getAllSongPlaylist(int userID);
}

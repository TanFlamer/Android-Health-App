package com.example.myapp.fragmentsMusic.expandableListMusic;

import java.util.List;

public class MusicExpandableListItem {

    public String getPlaylistName() {
        return playlistName;
    }

    public void setPlaylistName(String playlistName) {
        this.playlistName = playlistName;
    }

    public List<MusicExpandableListData> getMusicData() {
        return musicExpandableListDataList;
    }

    public void setMusicData(List<MusicExpandableListData> musicExpandableListDataList) {
        this.musicExpandableListDataList = musicExpandableListDataList;
    }

    String playlistName;
    List<MusicExpandableListData> musicExpandableListDataList;

    public MusicExpandableListItem(String playlistName, List<MusicExpandableListData> musicExpandableListDataList){
        this.playlistName = playlistName;
        this.musicExpandableListDataList = musicExpandableListDataList;
    }
}

package com.example.myapp.fragments.music.musicPlaylists;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.song.Song;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicPlaylistsAdapter extends BaseExpandableListAdapter {

    private final Context context;
    private final List<Playlist> playlistList;
    private final HashMap<Playlist, List<Song>> songPlaylists;
    private final HashMap<Playlist, Boolean> buttonMap;
    private final MusicPlaylistsFragment musicPlaylistsFragment;

    public MusicPlaylistsAdapter(Context context, HashMap<Playlist, List<Song>> songPlaylists, MusicPlaylistsFragment musicPlaylistsFragment){
        this.context = context;
        this.playlistList = new ArrayList<>(songPlaylists.keySet());
        this.songPlaylists = songPlaylists;
        this.musicPlaylistsFragment = musicPlaylistsFragment;
        buttonMap = new HashMap<>();
        for (Playlist playlist : playlistList) buttonMap.put(playlist, false);
    }

    @Override
    public int getGroupCount() {
        return playlistList.size();
    }

    @Override
    public int getChildrenCount(int i) {
        return Objects.requireNonNull(songPlaylists.get(playlistList.get(i))).size();
    }

    @Override
    public Object getGroup(int i) {
        return songPlaylists.get(playlistList.get(i));
    }

    @Override
    public Object getChild(int i, int i1) {
        return Objects.requireNonNull(songPlaylists.get(playlistList.get(i))).get(i1);
    }

    @Override
    public long getGroupId(int i) {
        return i;
    }

    @Override
    public long getChildId(int i, int i1) {
        return i1;
    }

    @Override
    public boolean hasStableIds() {
        return true;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getGroupView(int i, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item, null);

        initialiseGroupView(currentItemView, i);
        return currentItemView;
    }

    public void initialiseGroupView(View view, int position){
        Playlist playlist = playlistList.get(position);
        initialiseGroupName(view, playlist);
        initialiseVisibleLayout(view, position);
        initialiseHiddenLayout(view, playlist);
        initialiseEditButton(view, playlist);
        initialiseDeleteButton(view, playlist);
    }

    public void initialiseGroupName(View view, Playlist playlist){
        TextView nameView = view.findViewById(R.id.musicPlaylistName);
        nameView.setText(playlist.getPlaylistName());
    }

    public void initialiseVisibleLayout(View view, int position){
        /*layoutVisible.setOnClickListener(v -> {
            if(musicPlaylistsFragment.getExpandableListView().isGroupExpanded(position))
                musicPlaylistsFragment.getExpandableListView().collapseGroup(position);
            else
                musicPlaylistsFragment.getExpandableListView().expandGroup(position);
        });*/
        /*layoutVisible.setOnLongClickListener(v -> {
            Playlist playlist = playlistList.get(position);
            buttonMap.put(playlist, Boolean.FALSE.equals(buttonMap.get(playlist)));
            notifyDataSetChanged();
            return false;
        });*/
    }

    public void onLongClick(int position){
        Playlist playlist = playlistList.get(position);
        buttonMap.put(playlist, Boolean.FALSE.equals(buttonMap.get(playlist)));
        notifyDataSetChanged();
    }

    public void initialiseHiddenLayout(View view, Playlist playlist){
        LinearLayout layoutVisible = view.findViewById(R.id.layoutVisible);
        LinearLayout layoutHidden = view.findViewById(R.id.layoutHidden);
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(playlist)) ? View.VISIBLE : View.GONE);
    }

    public void initialiseEditButton(View view, Playlist playlist){
        ImageView clickEdit = view.findViewById(R.id.clickEdit);
        clickEdit.setOnClickListener(v -> {
            MusicPlaylistsViewModel musicPlaylistsViewModel = musicPlaylistsFragment.getMusicPlaylistsViewModel();
            context.startActivity(musicPlaylistsViewModel.editPlaylist(playlist.getPlaylistName()));
        });
    }

    public void initialiseDeleteButton(View view, Playlist playlist){
        MusicPlaylistsViewModel musicPlaylistsViewModel = musicPlaylistsFragment.getMusicPlaylistsViewModel();
        ImageView clickDelete = view.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view1 -> musicPlaylistsViewModel.deletePlaylist(context, playlist).show());
    }

    @SuppressLint({"InflateParams", "SetTextI18n"})
    @Override
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item_data, null);

        initialiseChildView(currentItemView, i, i1);
        return currentItemView;
    }

    public void initialiseChildView(View view, int parent, int child){
        Song song = Objects.requireNonNull(songPlaylists.get(playlistList.get(parent))).get(child);
        initialiseChildName(view, song);
        initialiseChildLength(view, song);
        initialiseOnClickView(view, parent, child);
    }

    public void initialiseChildName(View view, Song song){
        TextView nameView = view.findViewById(R.id.musicSongName);
        nameView.setText(song.getSongName());
    }

    @SuppressLint("SetTextI18n")
    public void initialiseChildLength(View view, Song song){
        TextView lengthView = view.findViewById(R.id.musicSongLength);
        lengthView.setText(song.getSongDuration().toString());
    }

    public void initialiseOnClickView(View view, int parent, int child){
        MusicPlayer musicPlayer = musicPlaylistsFragment.getMusicPlaylistsViewModel().getMusicPlayer();
        view.setOnClickListener(v -> musicPlayer.setPlaylist(songPlaylists.get(playlistList.get(parent)), child));
    }

    @Override
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }

    public void updateMusicPlaylists(HashMap<Playlist, List<Song>> newSongPlaylists, String data, String order){
        playlistList.clear();
        playlistList.addAll(newSongPlaylists.keySet());
        songPlaylists.clear();
        songPlaylists.putAll(newSongPlaylists);
        sortMusicPlaylists(data, order);
    }

    public void sortMusicPlaylists(String data, String order){
        //MusicPlaylistsViewModel musicPlaylistsViewModel = musicPlaylistsFragment.getMusicPlaylistsViewModel();
        playlistList.sort(getPlaylistComparator(data, order));
        for(List<Song> songList : songPlaylists.values()) songList.sort(getSongComparator(data, order));
        for(Playlist playlist : playlistList) buttonMap.put(playlist, false);
        notifyDataSetChanged();
    }

    public Comparator<Playlist> getPlaylistComparator(String data, String order){
        Comparator<Playlist> playlistComparator = Comparator.comparingInt(Playlist::getPlaylistID);
        switch (data) {
            case "Date Added":
                playlistComparator = Comparator.comparingInt(Playlist::getPlaylistID);
                break;
            case "Name":
                playlistComparator = Comparator.comparing(Playlist::getPlaylistName);
                break;
            case "Length":
                playlistComparator = Comparator.comparing(this::getPlaylistLength);
                break;
        }
        return order.equals("Ascending") ? playlistComparator : playlistComparator.reversed();
    }

    public Comparator<Song> getSongComparator(String data, String order){
        Comparator<Song> songComparator = Comparator.comparingInt(Song::getSongID);
        switch (data) {
            case "Date Added":
                songComparator = Comparator.comparingInt(Song::getSongID);
                break;
            case "Name":
                songComparator = Comparator.comparing(Song::getSongName);
                break;
            case "Length":
                songComparator = Comparator.comparing(Song::getSongDuration);
                break;
        }
        return order.equals("Ascending") ? songComparator : songComparator.reversed();
    }


    public int getPlaylistLength(Playlist playlist){
        int duration = 0;
        for(Song song : Objects.requireNonNull(songPlaylists.get(playlist)))
            duration += song.getSongDuration();
        return duration;
    }
}

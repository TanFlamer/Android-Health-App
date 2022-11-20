package com.example.myapp.fragmentsMusic.expandableListMusic;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

import com.example.myapp.R;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListItem;

import java.util.List;

public class MusicExpandableListAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<MusicExpandableListItem> musicExpandableListItemList;

    public MusicExpandableListAdapter(Context context, List<MusicExpandableListItem> musicExpandableListItemList){
        this.context = context;
        this.musicExpandableListItemList = musicExpandableListItemList;
    }

    @Override
    public int getGroupCount() {
        return musicExpandableListItemList.size();
    }

    @Override
    public int getChildrenCount(int i) {
        return musicExpandableListItemList.get(i).getMusicData().size();
    }

    @Override
    public Object getGroup(int i) {
        return musicExpandableListItemList.get(i).getMusicData();
    }

    @Override
    public Object getChild(int i, int i1) {
        return musicExpandableListItemList.get(i).getMusicData().get(i1);
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
        String playlistName = musicExpandableListItemList.get(i).getPlaylistName();

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item, null);

        TextView nameView = view.findViewById(R.id.musicPlaylistName);
        nameView.setText(playlistName);

        return view;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        MusicExpandableListData musicExpandableListData = musicExpandableListItemList.get(i).getMusicData().get(i1);

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item_data, null);

        TextView nameView = view.findViewById(R.id.musicSongName);
        TextView lengthView = view.findViewById(R.id.musicSongLength);

        nameView.setText(musicExpandableListData.getName());
        lengthView.setText(String.valueOf(musicExpandableListData.getLength()));

        return view;
    }

    @Override
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }

    public void updateMusicPlaylists(List<MusicExpandableListItem> newMusicExpandableListItemList){
        musicExpandableListItemList.clear();
        musicExpandableListItemList.addAll(newMusicExpandableListItemList);
        notifyDataSetChanged();
    }
}

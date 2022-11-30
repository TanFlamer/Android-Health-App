package com.example.myapp.mainActivities.save;

import android.content.Context;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class SaveListAdapter extends ArrayAdapter<Pair<String, LocalDateTime>> {

    private final List<Pair<String, LocalDateTime>> saveLogs;

    public SaveListAdapter(@NonNull Context context, int resource, List<Pair<String, LocalDateTime>> saveLogs) {
        super(context, resource, saveLogs);
        this.saveLogs = saveLogs;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.save_list_item, parent, false);

        initialiseAll(currentItemView, getItem(position));
        return currentItemView;
    }

    public void initialiseAll(View view, Pair<String, LocalDateTime> saveLog){
        String formattedDateTime = saveLog.second.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        initialiseLogView(view, saveLog.first);
        initialiseTimeView(view, formattedDateTime);
    }

    public void initialiseLogView(View view, String log){
        TextView logView = view.findViewById(R.id.saveLog);
        logView.setText(log);
    }

    public void initialiseTimeView(View view, String time){
        TextView timeView = view.findViewById(R.id.saveTime);
        timeView.setText(time);
    }

    public void updateSaveLogs(Pair<String, LocalDateTime> newSaveLogs){
        saveLogs.add(newSaveLogs);
        notifyDataSetChanged();
    }
}

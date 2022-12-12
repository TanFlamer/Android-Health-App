package com.example.myapp.mainActivities.logs;

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
import com.example.myapp.ViewHolder;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class LogsListAdapter extends ArrayAdapter<Pair<String, LocalDateTime>> {

    //logs list
    private final List<Pair<String, LocalDateTime>> saveLogs;

    //constructor for logs list adapter
    public LogsListAdapter(@NonNull Context context, int resource, List<Pair<String, LocalDateTime>> saveLogs) {
        super(context, resource, saveLogs);
        this.saveLogs = saveLogs;
    }

    @NonNull
    @Override //get view for each log
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        //inflate new view for log if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.logs_list_item, parent, false);
            //create new view holder
            ViewHolder viewHolder = new ViewHolder();
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.saveLog));
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.saveTime));
            //set tag to view
            currentItemView.setTag(viewHolder);
        }

        //get view holder
        ViewHolder viewHolder = (ViewHolder) currentItemView.getTag();
        //update log view data
        updateAllViews(viewHolder, getItem(position));
        //return log view
        return currentItemView;
    }

    //update log view data
    public void updateAllViews(ViewHolder viewHolder, Pair<String, LocalDateTime> saveLog){
        //update date time format
        String formattedDateTime = saveLog.second.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        //update log text
        updateLogView(viewHolder, saveLog.first);
        //update log date time
        updateTimeView(viewHolder, formattedDateTime);
    }

    //update log text
    public void updateLogView(ViewHolder viewHolder, String log){
        //get textview ID for log text
        TextView logView = (TextView) viewHolder.getView(R.id.saveLog);
        //set log text
        logView.setText(log);
    }

    //update log date time
    public void updateTimeView(ViewHolder viewHolder, String time){
        //get textview ID for log date time
        TextView timeView = (TextView) viewHolder.getView(R.id.saveTime);
        //set log date time
        timeView.setText(time);
    }

    //update logs list when new log appears
    public void updateSaveLogs(List<Pair<String, LocalDateTime>> newSaveLogs){
        //clear old logs list
        saveLogs.clear();
        //add new logs list
        saveLogs.addAll(newSaveLogs);
        //notify adapter that dataset changed
        notifyDataSetChanged();
    }
}
